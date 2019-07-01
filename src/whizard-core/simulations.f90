! WHIZARD 2.2.1 June 3 2014
! 
! Copyright (C) 1999-2014 by 
!     Wolfgang Kilian <kilian@physik.uni-siegen.de>
!     Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
!     Juergen Reuter <juergen.reuter@desy.de>
!     
!     with contributions from
!     Christian Speckner <cnspeckn@googlemail.com> 
!     and  Fabian Bach, Felix Braam, Sebastian Schmidt, Daniel Wiesler 
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

module simulations

  use kinds, only: default !NODEP!
  use iso_varying_string, string_t => varying_string !NODEP!
  use file_utils !NODEP!
  use limits, only: FMT_19 !NODEP!
  use diagnostics !NODEP!
  use unit_tests
  use sm_qcd
  use md5
  use ifiles
  use lexers
  use parser
  use variables
  use expressions
  use flavors
  use particles
  use state_matrices
  use interactions
  use models
  use beams
  use phs_forests
  use rng_base
  use selectors
  use prc_core
  use prclib_stacks
  use processes
  use events
  use event_transforms
  use decays
  use eio_data
  use eio_base
  use eio_raw
  use eio_ascii
  use rt_data
  use dispatch
  use process_configurations
  use compilations
  use integrations
  use event_streams

  implicit none
  private

  public :: simulation_t
  public :: simulations_test

  type :: counter_t
     integer :: total = 0
     integer :: generated = 0
     integer :: read = 0
     integer :: positive = 0
     integer :: negative = 0
     integer :: zero = 0
     integer :: excess = 0
     real(default) :: max_excess = 0
     real(default) :: sum_excess = 0
   contains
     procedure :: write => counter_write
     procedure :: show_excess => counter_show_excess
     procedure :: record => counter_record
  end type counter_t
  
  type :: mci_set_t
     private
     integer :: n_components = 0
     integer, dimension(:), allocatable :: i_component
     type(string_t), dimension(:), allocatable :: component_id
     logical :: has_integral = .false.
     real(default) :: integral = 0
     real(default) :: error = 0
     real(default) :: weight_mci = 0
     type(counter_t) :: counter
   contains
     procedure :: write => mci_set_write
     procedure :: init => mci_set_init
  end type mci_set_t
     
  type :: core_safe_t
     class(prc_core_t), allocatable :: core
  end type core_safe_t
  
  type, extends (event_t) :: entry_t
     private
     type(string_t) :: process_id
     type(string_t) :: library
     type(string_t) :: run_id
     logical :: has_integral = .false.
     real(default) :: integral = 0
     real(default) :: error = 0
     real(default) :: process_weight = 0
     logical :: valid = .false.
     type(counter_t) :: counter
     integer :: n_in = 0
     integer :: n_mci = 0
     type(mci_set_t), dimension(:), allocatable :: mci_set
     type(selector_t) :: mci_selector
     type(core_safe_t), dimension(:), allocatable :: core_safe
     type(model_t), pointer :: model => null ()
     type(qcd_t) :: qcd
   contains
     procedure :: write_config => entry_write_config
     procedure :: final => entry_final
     procedure :: init => entry_init
     procedure :: init_mci_selector => entry_init_mci_selector
     procedure :: select_mci => entry_select_mci
     procedure :: record => entry_record
     procedure :: update_process => entry_update_process
     procedure :: restore_process => entry_restore_process
  end type entry_t

  type, extends (entry_t) :: alt_entry_t
   contains
     procedure :: init_alt => alt_entry_init
     procedure :: fill_particle_set => entry_fill_particle_set
  end type alt_entry_t
  
  type :: simulation_t
     private
     type(string_t) :: sample_id
     logical :: unweighted = .true.
     logical :: negative_weights = .false.
     integer :: norm_mode = NORM_UNDEFINED
     logical :: pacify = .false.
     integer :: n_max_tries = 10000
     integer :: n_prc = 0
     integer :: n_alt = 0
     logical :: has_integral = .false.
     logical :: valid
     real(default) :: integral = 0
     real(default) :: error = 0
     integer :: version = 1
     character(32) :: md5sum_prc = ""
     character(32) :: md5sum_cfg = ""
     character(32), dimension(:), allocatable :: md5sum_alt
     type(entry_t), dimension(:), allocatable :: entry
     type(alt_entry_t), dimension(:,:), allocatable :: alt_entry
     type(selector_t) :: process_selector
     integer :: n_evt_requested = 0
     integer :: split_n_evt = 0
     integer :: split_index = 0
     type(counter_t) :: counter
     class(rng_t), allocatable :: rng
     integer :: i_prc = 0
     integer :: i_mci = 0
     real(default) :: weight = 0
     real(default) :: excess = 0
   contains
     procedure :: write => simulation_write
     generic :: write_event => write_event_unit
     procedure :: write_event_unit => simulation_write_event_unit
     procedure :: write_alt_event => simulation_write_alt_event
     procedure :: final => simulation_final
     procedure :: init => simulation_init
     procedure :: compute_n_events => simulation_compute_n_events
     procedure :: compute_md5sum => simulation_compute_md5sum
     procedure :: init_process_selector => simulation_init_process_selector
     procedure :: select_prc => simulation_select_prc
     procedure :: select_mci => simulation_select_mci
     procedure :: generate => simulation_generate
     procedure :: calculate_alt_entries => simulation_calculate_alt_entries
     procedure :: rescan => simulation_rescan
     procedure :: update_processes => simulation_update_processes
     procedure :: restore_processes => simulation_restore_processes
     generic :: write_event => write_event_eio
     procedure :: write_event_eio => simulation_write_event_eio
     generic :: read_event => read_event_eio
     procedure :: read_event_eio => simulation_read_event_eio
     generic :: write_event => write_event_es_array
     procedure :: write_event_es_array => simulation_write_event_es_array
     generic :: read_event => read_event_es_array
     procedure :: read_event_es_array => simulation_read_event_es_array
     procedure :: recalculate => simulation_recalculate
     procedure :: get_process_ptr => simulation_get_process_ptr
     procedure :: get_md5sum_prc => simulation_get_md5sum_prc
     procedure :: get_md5sum_cfg => simulation_get_md5sum_cfg
     procedure :: get_md5sum_alt => simulation_get_md5sum_alt
     procedure :: get_data => simulation_get_data
     procedure :: get_default_sample_name => simulation_get_default_sample_name
     procedure :: is_valid => simulation_is_valid
  end type simulation_t
  

  interface pacify
     module procedure pacify_simulation
  end interface

contains

  subroutine counter_write (object, unit)
    class(counter_t), intent(in) :: object
    integer, intent(in), optional :: unit
    integer :: u
    u = output_unit (unit)
1   format (3x,A,I0)
2   format (5x,A,I0)
3   format (5x,A,ES19.12)
    write (u, 1)  "Events total      = ", object%total
    write (u, 2)  "generated       = ", object%generated
    write (u, 2)  "read            = ", object%read
    write (u, 2)  "positive weight = ", object%positive
    write (u, 2)  "negative weight = ", object%negative
    write (u, 2)  "zero weight     = ", object%zero
    write (u, 2)  "excess weight   = ", object%excess
    if (object%excess /= 0) then
       write (u, 3)  "max excess      = ", object%max_excess
       write (u, 3)  "avg excess      = ", object%sum_excess / object%total
    end if
  end subroutine counter_write

  subroutine counter_show_excess (counter)
    class(counter_t), intent(in) :: counter
    if (counter%excess > 0) then
       write (msg_buffer, "(A,1x,I0,1x,A,1x,'(',F7.3,' %)')") &
            "Encountered events with excess weight:", counter%excess, &
            "events", 100 * counter%excess / real (counter%total)
       call msg_warning ()
       write (msg_buffer, "(A,ES10.3)") &
            "Maximum excess weight =", counter%max_excess
       call msg_message ()
       write (msg_buffer, "(A,ES10.3)") &
            "Average excess weight =", counter%sum_excess / counter%total
       call msg_message ()
    end if
  end subroutine counter_show_excess
    
  subroutine counter_record (counter, weight, excess, from_file)
    class(counter_t), intent(inout) :: counter
    real(default), intent(in), optional :: weight, excess
    logical, intent(in), optional :: from_file
    counter%total = counter%total + 1
    if (present (from_file)) then
       if (from_file) then
          counter%read = counter%read + 1
       else
          counter%generated = counter%generated + 1
       end if
    else
       counter%generated = counter%generated + 1
    end if
    if (present (weight)) then
       if (weight > 0) then
          counter%positive = counter%positive + 1
       else if (weight < 0) then
          counter%negative = counter%negative + 1
       else
          counter%zero = counter%zero + 1
       end if
    else
       counter%positive = counter%positive + 1
    end if
    if (present (excess)) then
       if (excess > 0) then
          counter%excess = counter%excess + 1
          counter%max_excess = max (counter%max_excess, excess)
          counter%sum_excess = counter%sum_excess + excess
       end if
    end if
  end subroutine counter_record
    
  subroutine mci_set_write (object, unit)
    class(mci_set_t), intent(in) :: object
    integer, intent(in), optional :: unit
    integer :: u, i
    u = output_unit (unit)
    write (u, "(3x,A)")  "Components:"
    do i = 1, object%n_components
       write (u, "(5x,I0,A,A,A)")  object%i_component(i), &
            ": '", char (object%component_id(i)), "'"
    end do
    if (object%has_integral) then
       write (u, "(3x,A," // FMT_19 // ")")  "Integral  = ", object%integral
       write (u, "(3x,A," // FMT_19 // ")")  "Error     = ", object%error
       write (u, "(3x,A,F13.10)")  "Weight    =", object%weight_mci
    else
       write (u, "(3x,A)")  "Integral  = [undefined]"
    end if
    call object%counter%write (u)
  end subroutine mci_set_write
  
  subroutine mci_set_init (object, i_mci, process)
    class(mci_set_t), intent(out) :: object
    integer, intent(in) :: i_mci
    type(process_t), intent(in), target :: process
    integer :: i
    call process%get_i_component (i_mci, object%i_component)
    object%n_components = size (object%i_component)
    allocate (object%component_id (object%n_components))
    do i = 1, size (object%component_id)
       object%component_id(i) = &
            process%get_component_id (object%i_component(i))
    end do
    if (process%has_integral (i_mci)) then
       object%integral = process%get_integral (i_mci)
       object%error = process%get_error (i_mci)
       object%has_integral = .true.
    end if
  end subroutine mci_set_init
    
  subroutine prepare_process (process, process_id, integrate, global)
    type(process_t), pointer, intent(out) :: process
    type(string_t), intent(in) :: process_id
    logical, intent(in) :: integrate
    type(rt_data_t), intent(inout), target :: global
    process => global%process_stack%get_process_ptr (process_id)
    if (.not. associated (process)) then
       if (integrate) then
          call msg_message ("Simulate: process '" &
               // char (process_id) // "' needs integration")
       else
          call msg_message ("Simulate: process '" &
               // char (process_id) // "' needs initialization")
       end if
       call integrate_process (process_id, global, init_only = .not. integrate)
       if (signal_is_pending ())  return
       process => global%process_stack%get_process_ptr (process_id)
       if (associated (process)) then
          if (integrate) then
             call msg_message ("Simulate: integration done")
             call global%process_stack%fill_result_vars (process_id)
          else
             call msg_message ("Simulate: process initialization done")
          end if
       else
          call msg_fatal ("Simulate: process '" &
               // char (process_id) // "' could not be initialized: aborting")
       end if
    end if
  end subroutine prepare_process
    
  subroutine entry_write_config (object, unit)
    class(entry_t), intent(in) :: object
    integer, intent(in), optional :: unit
    integer :: u, i
    u = output_unit (unit)
    write (u, "(3x,A,A,A)")  "Process   = '", char (object%process_id), "'"
    write (u, "(3x,A,A,A)")  "Library   = '", char (object%library), "'"
    write (u, "(3x,A,A,A)")  "Run       = '", char (object%run_id), "'"
    write (u, "(3x,A,L1)")   "is valid  = ", object%valid
    if (object%has_integral) then
       write (u, "(3x,A," // FMT_19 // ")")  "Integral  = ", object%integral
       write (u, "(3x,A," // FMT_19 // ")")  "Error     = ", object%error
       write (u, "(3x,A,F13.10)")  "Weight    =", object%process_weight
    else
       write (u, "(3x,A)")  "Integral  = [undefined]"
    end if
    write (u, "(3x,A,I0)")   "MCI sets  = ", object%n_mci
    call object%counter%write (u)
    do i = 1, size (object%mci_set)
       write (u, "(A)")
       write (u, "(1x,A,I0,A)")  "MCI set #", i, ":"
       call object%mci_set(i)%write (u)
    end do
    if (allocated (object%core_safe)) then
       do i = 1, size (object%core_safe)
          write (u, "(1x,A,I0,A)")  "Saved process-component core #", i, ":"
          call object%core_safe(i)%core%write (u)
       end do
    end if
  end subroutine entry_write_config
  
  subroutine entry_final (object)
    class(entry_t), intent(inout) :: object
    integer :: i
    if (associated (object%instance)) then
       do i = 1, object%n_mci
          call object%instance%final_simulation (i)
       end do
       call object%instance%final ()
       deallocate (object%instance)
    end if
    call object%event_t%final ()
  end subroutine entry_final
  
  subroutine entry_init (entry, process_id, integrate, generate, global, n_alt)
    class(entry_t), intent(inout), target :: entry
    type(string_t), intent(in) :: process_id
    logical, intent(in) :: integrate, generate
    type(rt_data_t), intent(inout), target :: global
    integer, intent(in), optional :: n_alt
    type(process_t), pointer :: process
    type(process_instance_t), pointer :: process_instance
    class(evt_t), pointer :: evt
    integer :: i
    logical :: enable_qcd
    
    enable_qcd = var_list_get_lval (global%var_list, var_str ("?ps_isr_active")) &
            .or. var_list_get_lval (global%var_list, var_str ("?ps_fsr_active")) &
            .or. var_list_get_lval (global%var_list, var_str &
                     ("?hadronization_active")) &
            .or. var_list_get_lval (global%var_list, var_str ("?mlm_matching")) &
            .or. var_list_get_lval (global%var_list, var_str ("?ckkw_matching")) &
            .or. var_list_get_lval (global%var_list, var_str ("?muli_active"))
    
    call prepare_process (process, process_id, integrate, global)
    if (signal_is_pending ())  return

    if (.not. process%has_matrix_element ()) then
       entry%has_integral = .true.
       entry%process_id = process_id
       entry%valid = .false.          
       return
    end if
    
    call entry%basic_init (global%var_list, n_alt)

    allocate (process_instance)
    call process_instance%init (process)
    call process_instance%setup_event_data ()

    entry%process_id = process_id
    entry%library = process%get_library_name ()
    entry%run_id = process%get_run_id ()
    entry%n_in = process%get_n_in ()
    entry%n_mci = process%get_n_mci ()
    allocate (entry%mci_set (entry%n_mci))
    do i = 1, size (entry%mci_set)
       call entry%mci_set(i)%init (i, process)
    end do
    if (process%has_integral ()) then
       entry%integral = process%get_integral ()
       entry%error = process%get_error ()
       call entry%set_sigma (entry%integral)
       entry%has_integral = .true.
    end if

    call entry%set_selection (global%pn%selection_lexpr)
    call entry%set_reweight (global%pn%reweight_expr)
    call entry%set_analysis (global%pn%analysis_lexpr)
    if (generate) then
       do i = 1, entry%n_mci
          call process%prepare_simulation (i)
          call process_instance%init_simulation (i, entry%config%safety_factor)
       end do
    end if

    if (process%contains_unstable (global%model)) then
       call dispatch_evt_decay (evt, global)
       if (associated (evt))  call entry%import_transform (evt)
    end if
    
    if (enable_qcd) then 
       call dispatch_evt_shower (evt, global, process)
       if (associated (evt))  call entry%import_transform (evt)
    end if

    call entry%connect (process_instance, global%model, global%process_stack)
    call entry%setup_expressions ()
    entry%model => process%get_model_ptr ()
    call dispatch_qcd (entry%qcd, global)
    entry%valid = .true.
    
  end subroutine entry_init
    
  subroutine entry_init_mci_selector (entry)
    class(entry_t), intent(inout) :: entry
    integer :: i
    if (entry%has_integral) then
       call entry%mci_selector%init (entry%mci_set%integral)
       do i = 1, entry%n_mci
          entry%mci_set(i)%weight_mci = entry%mci_selector%get_weight (i)
       end do
    end if
  end subroutine entry_init_mci_selector
  
  function entry_select_mci (entry) result (i_mci)
    class(entry_t), intent(inout) :: entry
    integer :: i_mci
    call entry%mci_selector%generate (entry%rng, i_mci)
  end function entry_select_mci
  
  subroutine entry_record (entry, i_mci, from_file)
    class(entry_t), intent(inout) :: entry
    integer, intent(in) :: i_mci
    logical, intent(in), optional :: from_file
    real(default) :: weight, excess
    weight = entry%weight_prc
    excess = entry%excess_prc
    call entry%counter%record (weight, excess, from_file)
    call entry%mci_set(i_mci)%counter%record (weight, excess)
  end subroutine entry_record
    
  subroutine entry_update_process (entry, model, qcd, helicity_selection)
    class(entry_t), intent(inout) :: entry
    type(model_t), intent(in), optional, target :: model
    type(qcd_t), intent(in), optional :: qcd
    type(helicity_selection_t), intent(in), optional :: helicity_selection
    type(process_t), pointer :: process
    class(prc_core_t), allocatable :: core
    integer :: i, n_components
    type(model_t), pointer :: model_local
    type(qcd_t) :: qcd_local
    if (present (model)) then
       model_local => model
    else
       model_local => entry%model
    end if
    if (present (qcd)) then
       qcd_local = qcd
    else
       qcd_local = entry%qcd
    end if
    process => entry%get_process_ptr ()
    n_components = process%get_n_components ()
    allocate (entry%core_safe (n_components))
    do i = 1, n_components
       if (process%has_matrix_element (i)) then
          call process%extract_component_core (i, core)
          call dispatch_core_update (core, &
               model_local, helicity_selection, qcd_local, &
               entry%core_safe(i)%core)
          call process%restore_component_core (i, core)
       end if
    end do
  end subroutine entry_update_process
  
  subroutine entry_restore_process (entry)
    class(entry_t), intent(inout) :: entry
    type(process_t), pointer :: process
    class(prc_core_t), allocatable :: core
    integer :: i, n_components
    process => entry%get_process_ptr ()
    n_components = process%get_n_components ()
    do i = 1, n_components
       if (process%has_matrix_element (i)) then
          call process%extract_component_core (i, core)
          call dispatch_core_restore (core, entry%core_safe(i)%core)
          call process%restore_component_core (i, core)
       end if
    end do
    deallocate (entry%core_safe)
  end subroutine entry_restore_process
  
  subroutine alt_entry_init (entry, process_id, &
       master_process, global)
    class(alt_entry_t), intent(inout), target :: entry
    type(string_t), intent(in) :: process_id
    type(process_t), intent(in), target :: master_process
    type(rt_data_t), intent(inout), target :: global
    class(rng_factory_t), allocatable :: rng_factory
    type(process_t), pointer :: process
    type(process_instance_t), pointer :: process_instance
    class(evt_t), pointer :: evt
    type(string_t) :: run_id
    type(integration_t) :: intg
    integer :: i
    logical :: enable_qcd
    
    enable_qcd = var_list_get_lval (global%var_list, var_str ("?ps_isr_active")) &
            .or. var_list_get_lval (global%var_list, var_str ("?ps_fsr_active")) &
            .or. var_list_get_lval (global%var_list, var_str &
                     ("?hadronization_active")) &
            .or. var_list_get_lval (global%var_list, var_str ("?mlm_matching")) &
            .or. var_list_get_lval (global%var_list, var_str ("?ckkw_matching")) &
            .or. var_list_get_lval (global%var_list, var_str ("?muli_active"))    

    call msg_message ("Simulate: initializing alternate process setup ...")

    run_id = var_list_get_sval (global%var_list, var_str ("$run_id"))
    call var_list_set_log (global%var_list, var_str ("?rebuild_phase_space"), &
         .false., is_known = .true.)
    call var_list_set_log (global%var_list, var_str ("?check_phs_file"), &
         .false., is_known = .true.)
    call var_list_set_log (global%var_list, var_str ("?rebuild_grids"), &
         .false., is_known = .true.)
    
    call dispatch_qcd (entry%qcd, global)
    call dispatch_rng_factory (rng_factory, global)

    allocate (process)
    call process%init (process_id, run_id, global%prclib, &
         global%os_data, entry%qcd, rng_factory, global%model_list)
    call intg%setup_process (global, process)

    call entry%basic_init (global%var_list)
    
    allocate (process_instance)
    call process_instance%init (process)
    call process_instance%setup_event_data ()
    entry%process_id = process_id
    entry%library = process%get_library_name ()
    entry%run_id = run_id
    entry%n_mci = process%get_n_mci ()
    allocate (entry%mci_set (entry%n_mci))
    do i = 1, size (entry%mci_set)
       call entry%mci_set(i)%init (i, master_process)
    end do
    if (master_process%has_integral ()) then
       entry%integral = master_process%get_integral ()
       entry%error = master_process%get_error ()
       call entry%set_sigma (entry%integral)
       entry%has_integral = .true.
    end if
    
    call entry%set_selection (global%pn%selection_lexpr)
    call entry%set_reweight (global%pn%reweight_expr)
    call entry%set_analysis (global%pn%analysis_lexpr)

    if (process%contains_unstable (global%model)) then
       call dispatch_evt_decay (evt, global)
       if (associated (evt))  call entry%import_transform (evt)
    end if

    if (enable_qcd) then
       call dispatch_evt_shower (evt, global)
       if (associated (evt))  call entry%import_transform (evt)
    end if

    call entry%connect (process_instance, global%model, global%process_stack)
    call entry%setup_expressions ()

    entry%model => process%get_model_ptr ()

    call msg_message ("...  alternate process setup complete.")

  end subroutine alt_entry_init

  subroutine entry_fill_particle_set (alt_entry, entry)
    class(alt_entry_t), intent(inout) :: alt_entry
    class(entry_t), intent(in), target :: entry
    type(particle_set_t) :: pset
    call entry%get_particle_set_hard_proc (pset)
    call alt_entry%set_particle_set_hard_proc (pset)
    call particle_set_final (pset)
  end subroutine entry_fill_particle_set
    
  subroutine simulation_write (object, unit)
    class(simulation_t), intent(in) :: object
    integer, intent(in), optional :: unit
    integer :: u, i
    u = output_unit (unit)
    call write_separator_double (u)
    write (u, "(1x,A,A,A)")  "Event sample: '", char (object%sample_id), "'"
    write (u, "(3x,A,I0)")  "Processes    = ", object%n_prc
    if (object%n_alt > 0) then
       write (u, "(3x,A,I0)")  "Alt.wgts     = ", object%n_alt
    end if
    write (u, "(3x,A,L1)")  "Unweighted   = ", object%unweighted
    write (u, "(3x,A,A)")   "Event norm   = ", &
         char (event_normalization_string (object%norm_mode))
    write (u, "(3x,A,L1)")  "Neg. weights = ", object%negative_weights
    write (u, "(3x,A,L1)")  "Pacify       = ", object%pacify
    write (u, "(3x,A,I0)")  "Max. tries   = ", object%n_max_tries
    if (object%has_integral) then
       write (u, "(3x,A," // FMT_19 // ")")  "Integral     = ", object%integral
       write (u, "(3x,A," // FMT_19 // ")")  "Error        = ", object%error
    else
       write (u, "(3x,A)")  "Integral     = [undefined]"
    end if
    write (u, "(3x,A,L1)")  "Sim. valid   = ", object%valid
    write (u, "(3x,A,I0)")  "Ev.file ver. = ", object%version
    if (object%md5sum_prc /= "") then
       write (u, "(3x,A,A,A)")  "MD5 sum (proc)   = '", object%md5sum_prc, "'"
    end if
    if (object%md5sum_cfg /= "") then
       write (u, "(3x,A,A,A)")  "MD5 sum (config) = '", object%md5sum_cfg, "'"
    end if
    write (u, "(3x,A,I0)")  "Events requested  = ", object%n_evt_requested
    if (object%split_n_evt > 0) then
       write (u, "(3x,A,I0)")  "Events per file   = ", object%split_n_evt
       write (u, "(3x,A,I0)")  "First file index  = ", object%split_index
    end if
    call object%counter%write (u)
    call write_separator (u)
    if (object%i_prc /= 0) then
       write (u, "(1x,A)")  "Current event:"
       write (u, "(3x,A,I0,A,A)")  "Process #", &
            object%i_prc, ": ", &
            char (object%entry(object%i_prc)%process_id)
       write (u, "(3x,A,I0)")  "MCI set #", object%i_mci
       write (u, "(3x,A," // FMT_19 // ")")  "Weight    = ", object%weight
       if (object%excess /= 0) &
            write (u, "(3x,A," // FMT_19 // ")")  "Excess    = ", object%excess
    else
       write (u, "(1x,A,I0,A,A)")  "Current event: [undefined]"
    end if
    call write_separator (u)
    if (allocated (object%rng)) then
       call object%rng%write (u)
    else
       write (u, "(3x,A)")  "Random-number generator: [undefined]"
    end if
    if (allocated (object%entry)) then
       do i = 1, size (object%entry)
          if (i == 1) then
             call write_separator_double (u)
          else
             call write_separator (u)
          end if
          write (u, "(1x,A,I0,A)") "Process #", i, ":"
          call object%entry(i)%write_config (u)
       end do
    end if
    call write_separator_double (u)
  end subroutine simulation_write
  
  subroutine simulation_write_event_unit (object, unit, i_prc, verbose, testflag)
    class(simulation_t), intent(in) :: object
    integer, intent(in), optional :: unit
    logical, intent(in), optional :: verbose
    integer, intent(in), optional :: i_prc
    logical, intent(in), optional :: testflag
    integer :: current
    if (present (i_prc)) then
       current = i_prc
    else
       current = object%i_prc
    end if
    if (current > 0) then
       call object%entry(current)%write (unit, verbose = verbose, &
            testflag = testflag)
    else
       call msg_fatal ("Simulation: write event: no process selected")
    end if
  end subroutine simulation_write_event_unit

  subroutine simulation_write_alt_event (object, unit, j_alt, i_prc, &
       verbose, testflag)
    class(simulation_t), intent(in) :: object
    integer, intent(in), optional :: unit
    integer, intent(in), optional :: j_alt
    integer, intent(in), optional :: i_prc
    logical, intent(in), optional :: verbose
    logical, intent(in), optional :: testflag
    integer :: i, j
    if (present (j_alt)) then
       j = j_alt
    else
       j = 1
    end if
    if (present (i_prc)) then
       i = i_prc
    else
       i = object%i_prc
    end if
    if (i > 0) then
       if (j> 0 .and. j <= object%n_alt) then
          call object%alt_entry(i,j)%write (unit, verbose = verbose, &
               testflag = testflag)
       else
          call msg_fatal ("Simulation: write alternate event: out of range")
       end if
    else
       call msg_fatal ("Simulation: write alternate event: no process selected")
    end if
  end subroutine simulation_write_alt_event

  subroutine simulation_final (object)
    class(simulation_t), intent(inout) :: object
    integer :: i, j
    if (allocated (object%entry)) then
       do i = 1, size (object%entry)
          call object%entry(i)%final ()
       end do
    end if
    if (allocated (object%alt_entry)) then
       do j = 1, size (object%alt_entry, 2)
          do i = 1, size (object%alt_entry, 1)
             call object%alt_entry(i,j)%final ()
          end do
       end do
    end if
    if (allocated (object%rng))  call object%rng%final ()
  end subroutine simulation_final
  
  subroutine simulation_init &
       (simulation, process_id, integrate, generate, global, alt_env)
    class(simulation_t), intent(out), target :: simulation
    type(string_t), dimension(:), intent(in) :: process_id
    logical, intent(in) :: integrate, generate
    type(rt_data_t), intent(inout), target :: global
    type(rt_data_t), dimension(:), intent(inout), target, optional :: alt_env
    class(rng_factory_t), allocatable :: rng_factory
    type(string_t) :: norm_string, version_string
    integer :: i, j
    simulation%sample_id = var_list_get_sval (global%var_list, &
         var_str ("$sample"))
    simulation%unweighted = var_list_get_lval (global%var_list, &
         var_str ("?unweighted"))
    simulation%negative_weights = var_list_get_lval (global%var_list, &
         var_str ("?negative_weights"))
    version_string = var_list_get_sval (global%var_list, &
         var_str ("$event_file_version"))
    norm_string = var_list_get_sval (global%var_list, &
         var_str ("$sample_normalization"))
    simulation%norm_mode = &
         event_normalization_mode (norm_string, simulation%unweighted)
    simulation%pacify = var_list_get_lval (global%var_list, &
         var_str ("?sample_pacify"))
    simulation%n_max_tries = var_list_get_ival (global%var_list, &
         var_str ("sample_max_tries"))
    simulation%split_n_evt = var_list_get_ival (global%var_list, &
         var_str ("sample_split_n_evt"))
    simulation%split_index = var_list_get_ival (global%var_list, &
         var_str ("sample_split_index"))
    select case (size (process_id))
    case (0)
       call msg_error ("Simulation: no process selected")
    case (1)
       write (msg_buffer, "(A,A,A)") &
            "Starting simulation for process '", &
            char (process_id(1)), "'"
       call msg_message ()
    case default
       write (msg_buffer, "(A,A,A)") &
            "Starting simulation for processes '", &
            char (process_id(1)), "' etc."
       call msg_message ()
    end select
    select case (char (version_string))
    case ("2.0","2.1")
       simulation%version = 0
    case default
       simulation%version = 1
    end select
    if (simulation%version == 0) then
       call msg_fatal ("Event file formats older than version 2.2 are " &
          // "not compatible with this version.")
    end if        
    simulation%n_prc = size (process_id)
    allocate (simulation%entry (simulation%n_prc))
    if (present (alt_env)) then
       simulation%n_alt = size (alt_env)
       do i = 1, simulation%n_prc
          call simulation%entry(i)%init &
               (process_id(i), integrate, generate, global, simulation%n_alt)
          if (signal_is_pending ())  return
       end do
       if (.not. any (simulation%entry%valid)) then
          call msg_error ("Simulate: no process has a valid matrix element.")
          simulation%valid = .false.
          return
       end if
       call simulation%update_processes ()
       allocate (simulation%alt_entry (simulation%n_prc, simulation%n_alt))
       allocate (simulation%md5sum_alt (simulation%n_alt))
       simulation%md5sum_alt = ""
       do j = 1, simulation%n_alt
          do i = 1, simulation%n_prc
             call simulation%alt_entry(i,j)%init_alt (process_id(i), &
                  simulation%entry(i)%get_process_ptr (), alt_env(j))
             if (signal_is_pending ())  return
          end do
       end do
       call simulation%restore_processes ()
    else       
       do i = 1, simulation%n_prc
          call simulation%entry(i)%init &
               (process_id(i), integrate, generate, global)
          if (signal_is_pending ())  return          
       end do
       if (.not. any (simulation%entry%valid)) then
          call msg_error ("Simulate: " &
               // "no process has a valid matrix element.") 
          simulation%valid = .false.
          return
       end if
    end if
    call dispatch_rng_factory (rng_factory, global)
    call rng_factory%make (simulation%rng)
    if (all (simulation%entry%has_integral)) then
       simulation%integral = sum (simulation%entry%integral)
       simulation%error = sqrt (sum (simulation%entry%error ** 2))
       simulation%has_integral = .true.
       if (integrate .and. generate) then
          do i = 1, simulation%n_prc
             if (simulation%entry(i)%integral < 0 .and. .not. &
                  simulation%negative_weights) then
                call msg_fatal ("Integral of process '" // &
                     char (process_id (i)) // "'is negative.")
             end if
          end do
       end if
    else
       if (integrate .and. generate) &
            call msg_error ("Simulation contains undefined integrals.")
    end if
    if (simulation%integral > 0 .or. &
         (simulation%integral < 0 .and. simulation%negative_weights)) then
       simulation%valid = .true.
    else if (generate) then
       call msg_error ("Simulate: " &
            // "sum of process integrals must be positive; skipping.")
       simulation%valid = .false.
       return
    end if
    if (simulation%valid)  call simulation%compute_md5sum ()
  end subroutine simulation_init

  subroutine simulation_compute_n_events (simulation, n_events, var_list)
    class(simulation_t), intent(in) :: simulation
    integer, intent(out) :: n_events
    type(var_list_t) :: var_list
    real(default) :: lumi, x_events_lumi
    integer :: n_events_lumi
    logical :: is_scattering
    n_events = var_list_get_ival (var_list, var_str ("n_events"))
    lumi = var_list_get_rval (var_list, var_str ("luminosity"))
    if (simulation%unweighted) then
       is_scattering = simulation%entry(1)%n_in == 2
       if (is_scattering) then
          x_events_lumi = abs (simulation%integral * lumi)
          if (x_events_lumi < huge (n_events)) then
             n_events_lumi = nint (x_events_lumi)
          else
             call msg_message ("Simulation: luminosity too large, &
                  &limiting number of events")
             n_events_lumi = huge (n_events)
          end if
          if (n_events_lumi > n_events) then
             call msg_message ("Simulation: using n_events as computed from &
                  &luminosity value")
             n_events = n_events_lumi
          else
             write (msg_buffer, "(A,1x,I0)") &
                  "Simulation: requested number of events =", n_events
             call msg_message ()
             write (msg_buffer, "(A,1x,ES11.4)") &
                  "            corr. to luminosity [fb-1] = ", &
                   n_events / simulation%integral            
             call msg_message ()
          end if
       end if
    end if
  end subroutine simulation_compute_n_events

  subroutine simulation_compute_md5sum (simulation)
    class(simulation_t), intent(inout) :: simulation
    type(process_t), pointer :: process
    type(string_t) :: buffer
    integer :: j, i, n_mci, i_mci, n_component, i_component
    if (simulation%md5sum_prc == "") then
       buffer = ""
       do i = 1, simulation%n_prc
          if (.not. simulation%entry(i)%valid) cycle
          process => simulation%entry(i)%get_process_ptr ()
          n_component = process%get_n_components ()
          do i_component = 1, n_component
             if (process%has_matrix_element (i_component)) then
                buffer = buffer // process%get_md5sum_prc (i_component)
             end if
          end do
       end do
       simulation%md5sum_prc = md5sum (char (buffer))
    end if
    if (simulation%md5sum_cfg == "") then
       buffer = ""
       do i = 1, simulation%n_prc
          if (.not. simulation%entry(i)%valid) cycle          
          process => simulation%entry(i)%get_process_ptr ()
          n_mci = process%get_n_mci ()
          do i_mci = 1, n_mci
             buffer = buffer // process%get_md5sum_mci (i_mci)
          end do
       end do
       simulation%md5sum_cfg = md5sum (char (buffer))
    end if
    do j = 1, simulation%n_alt
       if (simulation%md5sum_alt(j) == "") then
          buffer = ""
          do i = 1, simulation%n_prc
             process => simulation%alt_entry(i,j)%get_process_ptr ()
             buffer = buffer // process%get_md5sum_cfg ()
          end do
          simulation%md5sum_alt(j) = md5sum (char (buffer))
       end if
    end do
  end subroutine simulation_compute_md5sum

  subroutine simulation_init_process_selector (simulation)
    class(simulation_t), intent(inout) :: simulation
    integer :: i
    if (simulation%has_integral) then
       call simulation%process_selector%init (simulation%entry%integral)
       do i = 1, simulation%n_prc
          associate (entry => simulation%entry(i))
            if (.not. entry%valid) then
               call msg_warning ("Process '" // char (entry%process_id) // &
                    "': matrix element vanishes, no events can be generated.")
               cycle
            end if
            call entry%init_mci_selector ()
            entry%process_weight = simulation%process_selector%get_weight (i)
          end associate
       end do
    end if
  end subroutine simulation_init_process_selector
    
  function simulation_select_prc (simulation) result (i_prc)
    class(simulation_t), intent(inout) :: simulation
    integer :: i_prc
    call simulation%process_selector%generate (simulation%rng, i_prc)
  end function simulation_select_prc

  function simulation_select_mci (simulation) result (i_mci)
    class(simulation_t), intent(inout) :: simulation
    integer :: i_mci
    if (simulation%i_prc /= 0) then
       i_mci = simulation%entry(simulation%i_prc)%select_mci ()
    end if
  end function simulation_select_mci

  subroutine simulation_generate (simulation, n, es_array)
    class(simulation_t), intent(inout) :: simulation
    integer, intent(in) :: n
    type(event_stream_array_t), intent(inout), optional :: es_array
    type(string_t) :: str1, str2, str3
    logical :: generate_new
    integer :: i, j
    simulation%n_evt_requested = n
    call simulation%entry%set_n (n)
    if (simulation%n_alt > 0)  call simulation%alt_entry%set_n (n)
    str1 = "Events: generating"
    if (present (es_array)) then
       if (es_array%has_input ())  str1 = "Events: reading"
    end if
    if (simulation%entry(1)%config%unweighted) then
       str2 = "unweighted"
    else
       str2 = "weighted"
    end if
    if (simulation%entry(1)%config%factorization_mode == &
         FM_IGNORE_HELICITY) then
       str3 = ", unpolarized"
    else 
       str3 = ", polarized"
    end if    
    write (msg_buffer, "(A,1x,I0,1x,A,1x,A)")  char (str1), n, &
         char (str2) // char(str3), "events ..."
    call msg_message ()
    write (msg_buffer, "(A,1x,A)") "Events: event normalization mode", &
         char (event_normalization_string (simulation%norm_mode))
    call msg_message ()
    do i = 1, n
       if (present (es_array)) then
          call simulation%read_event (es_array, .true., generate_new)
       else
          generate_new = .true.
       end if
       if (generate_new) then
          simulation%i_prc = simulation%select_prc ()
          simulation%i_mci = simulation%select_mci ()
          associate (entry => simulation%entry(simulation%i_prc))
            do j = 1, simulation%n_max_tries
               if (.not. entry%valid)  call msg_warning &
                       ("Process '" // char (entry%process_id) // "': " // &
                       "matrix element vanishes, no events can be generated.")
               call entry%generate (simulation%i_mci)
               if (signal_is_pending ()) return
               if (entry%particle_set_exists)  exit
            end do
            if (.not. entry%particle_set_exists) then
               write (msg_buffer, "(A,I0,A)")  "Simulation: failed to &
                    &generate valid event after ", &
                    simulation%n_max_tries, " tries (sample_max_tries)"
               call msg_fatal ()
            end if
            call entry%evaluate_expressions ()
            if (signal_is_pending ()) return
            simulation%weight = entry%weight_ref
            simulation%excess = entry%excess_prc
            call simulation%counter%record &
                 (simulation%weight, simulation%excess)
            call entry%record (simulation%i_mci)
          end associate
       else
          associate (entry => simulation%entry(simulation%i_prc))
            call entry%accept_sqme_ref ()
            call entry%accept_weight_ref ()
            call entry%check ()
            call entry%evaluate_expressions ()
            if (signal_is_pending ()) return
            simulation%weight = entry%weight_ref
            simulation%excess = entry%excess_prc
            call simulation%counter%record &
                 (simulation%weight, simulation%excess, from_file=.true.)
            call entry%record (simulation%i_mci, from_file=.true.)
          end associate
       end if
       call simulation%calculate_alt_entries ()
       if (signal_is_pending ()) return
       if (simulation%pacify)  call pacify (simulation)
       if (present (es_array)) then
          call simulation%write_event (es_array)
       end if
    end do
    call msg_message ("        ... event sample complete.")
    call simulation%counter%show_excess ()
  end subroutine simulation_generate
  
  subroutine simulation_calculate_alt_entries (simulation)
    class(simulation_t), intent(inout) :: simulation
    real(default) :: factor
    real(default), dimension(:), allocatable :: sqme_alt, weight_alt
    integer :: n_alt, i, j
    i = simulation%i_prc
    n_alt = simulation%n_alt
    if (n_alt == 0)  return
    allocate (sqme_alt (n_alt), weight_alt (n_alt))
    associate (entry => simulation%entry(i))
      do j = 1, n_alt
         if (signal_is_pending ())  return
         factor = entry%get_kinematical_weight ()
         associate (alt_entry => simulation%alt_entry(i,j))
           call alt_entry%update_process ()
           call alt_entry%select &
                (entry%get_i_mci (), entry%get_i_term (), entry%get_channel ())
           call alt_entry%fill_particle_set (entry)
           call alt_entry%recalculate &
                (update_sqme = .true., weight_factor = factor)
           if (signal_is_pending ())  return
           call alt_entry%accept_sqme_prc ()
           call alt_entry%update_normalization ()
           call alt_entry%accept_weight_prc ()
           call alt_entry%check ()
           call alt_entry%evaluate_expressions ()
           if (signal_is_pending ())  return
           call alt_entry%restore_process ()
           sqme_alt(j) = alt_entry%sqme_ref
           weight_alt(j) = alt_entry%weight_ref
         end associate
      end do
      call entry%set (sqme_alt = sqme_alt, weight_alt = weight_alt)
      call entry%check ()
      call entry%store_alt_values ()
    end associate
  end subroutine simulation_calculate_alt_entries
       
  subroutine simulation_rescan &
       (simulation, n, es_array, update_event, update_sqme, update_weight, &
       recover_beams, global)
    class(simulation_t), intent(inout) :: simulation
    integer, intent(in) :: n
    type(event_stream_array_t), intent(inout) :: es_array
    logical, intent(in) :: update_event, update_sqme, update_weight
    logical, intent(in) :: recover_beams
    type(rt_data_t), intent(inout) :: global
    type(qcd_t) :: qcd
    type(string_t) :: str1, str2, str3
    logical :: complete
    str1 = "Rescanning"
    if (simulation%entry(1)%config%unweighted) then
       str2 = "unweighted"
    else
       str2 = "weighted"
    end if
    simulation%n_evt_requested = n
    call simulation%entry%set_n (n)
    if (update_sqme .or. update_weight) then
       call dispatch_qcd (qcd, global)
       call simulation%update_processes &
            (global%model, qcd, global%get_helicity_selection ())
       str3 = "(process parameters updated) "
    else
       str3 = ""
    end if
    write (msg_buffer, "(A,1x,A,1x,A,A,A)")  char (str1), char (str2), &
         "events ", char (str3), "..."
    call msg_message ()
    do
       call simulation%read_event (es_array, .false., complete)
       if (complete)  exit
       if (update_event .or. update_sqme .or. update_weight) then
          call simulation%recalculate (update_sqme, update_weight, &
               recover_beams)
          if (signal_is_pending ())  return
          associate (entry => simulation%entry(simulation%i_prc))
            call entry%update_normalization ()
            call entry%check ()
            call entry%evaluate_expressions ()
            if (signal_is_pending ())  return
            simulation%weight = entry%weight_prc
            call simulation%counter%record (simulation%weight, from_file=.true.)
            call entry%record (simulation%i_mci, from_file=.true.)
          end associate
       else
          associate (entry => simulation%entry(simulation%i_prc))
            call entry%accept_sqme_ref ()
            call entry%accept_weight_ref ()
            call entry%check ()
            call entry%evaluate_expressions ()
            if (signal_is_pending ())  return
            simulation%weight = entry%weight_ref
            call simulation%counter%record (simulation%weight, from_file=.true.)
            call entry%record (simulation%i_mci, from_file=.true.)
          end associate
       end if
       call simulation%calculate_alt_entries ()
       if (signal_is_pending ())  return
       call simulation%write_event (es_array)
    end do
    if (update_sqme .or. update_weight) then
       call simulation%restore_processes ()
    end if
  end subroutine simulation_rescan
  
  subroutine simulation_update_processes (simulation, &
       model, qcd, helicity_selection)
    class(simulation_t), intent(inout) :: simulation
    type(model_t), intent(in), optional, target :: model
    type(qcd_t), intent(in), optional :: qcd
    type(helicity_selection_t), intent(in), optional :: helicity_selection
    integer :: i
    do i = 1, simulation%n_prc
       call simulation%entry(i)%update_process (model, qcd, helicity_selection)
    end do
  end subroutine simulation_update_processes
  
  subroutine simulation_restore_processes (simulation)
    class(simulation_t), intent(inout) :: simulation
    integer :: i
    do i = 1, simulation%n_prc
       call simulation%entry(i)%restore_process ()
    end do
  end subroutine simulation_restore_processes
  
  subroutine simulation_write_event_eio (object, eio, i_prc)
    class(simulation_t), intent(in) :: object
    class(eio_t), intent(inout) :: eio
    integer, intent(in), optional :: i_prc
    integer :: current
    if (present (i_prc)) then
       current = i_prc
    else
       current = object%i_prc
    end if
    if (current > 0) then
       if (object%split_n_evt > 0) then
          if (object%counter%total > 1 .and. &
               mod (object%counter%total, object%split_n_evt) == 1) then
             call eio%split_out ()
          end if
       end if
       call eio%output (object%entry(current)%event_t, current)
    else
       call msg_fatal ("Simulation: write event: no process selected")
    end if
  end subroutine simulation_write_event_eio

  subroutine simulation_read_event_eio (object, eio)
    class(simulation_t), intent(inout) :: object
    class(eio_t), intent(inout) :: eio
    integer :: iostat, current
    call eio%input_i_prc (current, iostat)
    select case (iostat)
    case (0)
       object%i_prc = current
       call eio%input_event (object%entry(current)%event_t, iostat)
    end select
    select case (iostat)
    case (:-1)
       object%i_prc = 0
       object%i_mci = 0
    case (1:)
       call msg_error ("Reading events: I/O error, aborting read")
       object%i_prc = 0
       object%i_mci = 0
    case default
       object%i_mci = object%entry(current)%get_i_mci ()
    end select
  end subroutine simulation_read_event_eio

  subroutine simulation_write_event_es_array (object, es_array)
    class(simulation_t), intent(in) :: object
    class(event_stream_array_t), intent(inout) :: es_array
    integer :: i_prc, event_index
    i_prc = object%i_prc
    if (i_prc > 0) then
       event_index = object%counter%total
       call es_array%output (object%entry(i_prc)%event_t, i_prc, event_index)
    else
       call msg_fatal ("Simulation: write event: no process selected")
    end if
  end subroutine simulation_write_event_es_array

  subroutine simulation_read_event_es_array (object, es_array, enable_switch, &
       fail)
    class(simulation_t), intent(inout) :: object
    class(event_stream_array_t), intent(inout) :: es_array
    logical, intent(in) :: enable_switch
    logical, intent(out) :: fail
    integer :: iostat, i_prc
    if (es_array%has_input ()) then
       fail = .false.
       call es_array%input_i_prc (i_prc, iostat)
       select case (iostat)
       case (0)
          object%i_prc = i_prc
          call es_array%input_event (object%entry(i_prc)%event_t, iostat)
       case (:-1)
          write (msg_buffer, "(A,1x,I0,1x,A)")  &
               "... event file terminates after", &
               object%counter%read, "events."
          call msg_message ()
          if (enable_switch) then
             call es_array%switch_inout ()
             write (msg_buffer, "(A,1x,I0,1x,A)")  &
                  "Generating remaining ", &
                  object%n_evt_requested - object%counter%read, "events ..."
             call msg_message ()
          end if
          fail = .true.
          return
       end select
       select case (iostat)
       case (0)
          object%i_mci = object%entry(i_prc)%get_i_mci ()
       case default
          write (msg_buffer, "(A,1x,I0,1x,A)")  &
               "Reading events: I/O error, aborting read after", &
               object%counter%read, "events."
          call msg_error ()
          object%i_prc = 0
          object%i_mci = 0
          fail = .true.
       end select
    else
       fail = .true.
    end if
  end subroutine simulation_read_event_es_array

  subroutine simulation_recalculate (simulation, update_sqme, update_weight, &
       recover_beams)
    class(simulation_t), intent(inout) :: simulation
    logical, intent(in) :: update_sqme, update_weight
    logical, intent(in), optional :: recover_beams
    integer :: i_prc
    i_prc = simulation%i_prc
    associate (entry => simulation%entry(i_prc))
      if (update_weight) then
         call simulation%entry(i_prc)%recalculate &
              (update_sqme = update_sqme, recover_beams = recover_beams, &
              weight_factor = entry%get_kinematical_weight ())
      else
         call simulation%entry(i_prc)%recalculate &
              (update_sqme = update_sqme, recover_beams = recover_beams)
      end if
    end associate
  end subroutine simulation_recalculate

  function simulation_get_process_ptr (simulation) result (ptr)
    class(simulation_t), intent(in) :: simulation
    type(process_ptr_t), dimension(:), allocatable :: ptr
    integer :: i
    allocate (ptr (simulation%n_prc))
    do i = 1, size (ptr)
       ptr(i)%ptr => simulation%entry(i)%get_process_ptr ()
    end do
  end function simulation_get_process_ptr
    
  function simulation_get_md5sum_prc (simulation) result (md5sum)
    class(simulation_t), intent(in) :: simulation
    character(32) :: md5sum
    md5sum = simulation%md5sum_prc
  end function simulation_get_md5sum_prc
    
  function simulation_get_md5sum_cfg (simulation) result (md5sum)
    class(simulation_t), intent(in) :: simulation
    character(32) :: md5sum
    md5sum = simulation%md5sum_cfg
  end function simulation_get_md5sum_cfg
    
  function simulation_get_md5sum_alt (simulation, i) result (md5sum)
    class(simulation_t), intent(in) :: simulation
    integer, intent(in) :: i
    character(32) :: md5sum
    md5sum = simulation%md5sum_alt(i)
  end function simulation_get_md5sum_alt
    
  function simulation_get_data (simulation, alt) result (data)
    class(simulation_t), intent(in) :: simulation
    logical, intent(in), optional :: alt
    type(event_sample_data_t) :: data
    type(process_t), pointer :: process
    type(beam_data_t), pointer :: beam_data
    integer :: n, i
    logical :: enable_alt
    enable_alt = .true.;  if (present (alt))  enable_alt = alt    
    process => simulation%entry(1)%get_process_ptr ()
    beam_data => process%get_beam_data_ptr ()
    if (enable_alt) then
       call data%init (simulation%n_prc, simulation%n_alt)
       do i = 1, simulation%n_alt
          data%md5sum_alt(i) = simulation%get_md5sum_alt (i)
       end do
    else
       call data%init (simulation%n_prc)
    end if
    data%unweighted = simulation%unweighted
    data%negative_weights = simulation%negative_weights
    data%norm_mode = simulation%norm_mode
    n = beam_data_get_n_in (beam_data)
    data%n_beam = n
    data%pdg_beam(:n) = flavor_get_pdg (beam_data_get_flavor (beam_data))
    data%energy_beam(:n) = beam_data_get_energy (beam_data)
    do i = 1, simulation%n_prc
       if (.not. simulation%entry(i)%valid) cycle
       process => simulation%entry(i)%get_process_ptr ()
       data%proc_num_id(i) = process%get_num_id ()
       if (data%proc_num_id(i) == 0)  data%proc_num_id(i) = i
       if (simulation%entry(i)%has_integral) then
          data%cross_section(i) = simulation%entry(i)%integral
          data%error(i) = simulation%entry(i)%error
       end if
    end do
    data%total_cross_section = sum (data%cross_section)
    data%md5sum_prc = simulation%get_md5sum_prc ()
    data%md5sum_cfg = simulation%get_md5sum_cfg ()
    if (simulation%split_n_evt > 0) then
       data%split_n_evt = simulation%split_n_evt
       data%split_index = simulation%split_index
    end if
  end function simulation_get_data
    
  function simulation_get_default_sample_name (simulation) result (sample)
    class(simulation_t), intent(in) :: simulation
    type(string_t) :: sample
    type(process_t), pointer :: process
    sample = "whizard"
    if (simulation%n_prc > 0) then
       process => simulation%entry(1)%get_process_ptr ()
       if (associated (process)) then
          sample = process%get_id ()
       end if
    end if
  end function simulation_get_default_sample_name

  function simulation_is_valid (simulation) result (valid)
    class(simulation_t), intent(inout) :: simulation
    logical :: valid
    valid = simulation%valid
  end function simulation_is_valid

  subroutine pacify_simulation (simulation)
    class(simulation_t), intent(inout) :: simulation
    integer :: i, j
    i = simulation%i_prc
    if (i > 0) then
       call pacify (simulation%entry(i))
       do j = 1, simulation%n_alt
          call pacify (simulation%alt_entry(i,j))
       end do
    end if
  end subroutine pacify_simulation
  

  subroutine simulations_test (u, results)
    integer, intent(in) :: u
    type(test_results_t), intent(inout) :: results
    call test (simulations_1, "simulations_1", &
         "initialization", &
         u, results)
    call test (simulations_2, "simulations_2", &
         "weighted events", &
         u, results)
    call test (simulations_3, "simulations_3", &
         "unweighted events", &
         u, results)
    call test (simulations_4, "simulations_4", &
         "process with structure functions", &
         u, results)
    call test (simulations_5, "simulations_5", &
         "raw event I/O", &
         u, results)
    call test (simulations_6, "simulations_6", &
         "raw event I/O with structure functions", &
         u, results)
    call test (simulations_7, "simulations_7", &
         "automatic raw event I/O", &
         u, results)
    call test (simulations_8, "simulations_8", &
         "rescan raw event file", &
         u, results)
    call test (simulations_9, "simulations_9", &
         "rescan mismatch", &
         u, results)
    call test (simulations_10, "simulations_10", &
         "alternative weight", &
         u, results)
    call test (simulations_11, "simulations_11", &
         "decay", &
         u, results)
    call test (simulations_12, "simulations_12", &
         "split event files", &
         u, results)
  end subroutine simulations_test

  subroutine simulations_1 (u)
    integer, intent(in) :: u
    type(string_t) :: libname, procname1, procname2
    type(rt_data_t), target :: global
    type(simulation_t), target :: simulation
    
    write (u, "(A)")  "* Test output: simulations_1"
    write (u, "(A)")  "*   Purpose: initialize simulation"
    write (u, "(A)")

    write (u, "(A)")  "* Initialize processes"
    write (u, "(A)")

    call syntax_model_file_init ()

    call global%global_init ()
    call var_list_set_log (global%var_list, var_str ("?omega_openmp"), &
         .false., is_known = .true.)
    call var_list_set_int (global%var_list, var_str ("seed"), &
         0, is_known = .true.)    
    
    libname = "simulation_1a"
    procname1 = "simulation_1p"
    
    call prepare_test_library (global, libname, 1, [procname1])
    call compile_library (libname, global)

    call var_list_set_string (global%var_list, var_str ("$method"), &
         var_str ("unit_test"), is_known = .true.)
    call var_list_set_string (global%var_list, var_str ("$phs_method"), &
         var_str ("single"), is_known = .true.)
    call var_list_set_string (global%var_list, var_str ("$integration_method"),&
         var_str ("midpoint"), is_known = .true.)
    call var_list_set_log (global%var_list, var_str ("?vis_history"),&
         .false., is_known = .true.)    
    call var_list_set_log (global%var_list, var_str ("?integration_timer"),&
         .false., is_known = .true.)    
    
    call var_list_set_real (global%var_list, var_str ("sqrts"),&
         1000._default, is_known = .true.)

    call global%it_list%init ([1], [1000])

    call var_list_set_string (global%var_list, var_str ("$run_id"), &
         var_str ("simulations1"), is_known = .true.)
    call integrate_process (procname1, global)

    libname = "simulation_1b"
    procname2 = "sim_extra"
    
    call prepare_test_library (global, libname, 1, [procname2])
    call compile_library (libname, global)
    call var_list_set_string (global%var_list, var_str ("$run_id"), &
         var_str ("simulations2"), is_known = .true.)


    write (u, "(A)")  "* Initialize event generation"
    write (u, "(A)")

    call var_list_set_string (global%var_list, var_str ("$sample"), &
         var_str ("sim1"), is_known = .true.)
    call integrate_process (procname2, global)

    call simulation%init ([procname1, procname2], .true., .true., global)
    call simulation%init_process_selector ()
    call simulation%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Write the event record for the first process"
    write (u, "(A)")
    
    call simulation%write_event (u, i_prc = 1)

    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"

    call simulation%final ()
    call global%final ()
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: simulations_1"
    
  end subroutine simulations_1
  
  subroutine simulations_2 (u)
    integer, intent(in) :: u
    type(string_t) :: libname, procname1
    type(rt_data_t), target :: global
    type(simulation_t), target :: simulation
    type(event_sample_data_t) :: data
    
    write (u, "(A)")  "* Test output: simulations_2"
    write (u, "(A)")  "*   Purpose: generate events for a single process"
    write (u, "(A)")

    write (u, "(A)")  "* Initialize processes"
    write (u, "(A)")

    call syntax_model_file_init ()

    call global%global_init ()
    call var_list_set_log (global%var_list, var_str ("?omega_openmp"), &
         .false., is_known = .true.)
    call var_list_set_int (global%var_list, var_str ("seed"), &
         0, is_known = .true.)    
    
    libname = "simulation_2a"
    procname1 = "simulation_2p"
    
    call prepare_test_library (global, libname, 1, [procname1])
    call compile_library (libname, global)

    call var_list_append_log (global%var_list, &
         var_str ("?rebuild_events"), .true., intrinsic = .true.)

    call var_list_set_string (global%var_list, var_str ("$method"), &
         var_str ("unit_test"), is_known = .true.)
    call var_list_set_string (global%var_list, var_str ("$phs_method"), &
         var_str ("single"), is_known = .true.)
    call var_list_set_string (global%var_list, var_str ("$integration_method"),&
         var_str ("midpoint"), is_known = .true.)
    call var_list_set_log (global%var_list, var_str ("?vis_history"),&
         .false., is_known = .true.)    
    call var_list_set_log (global%var_list, var_str ("?integration_timer"),&
         .false., is_known = .true.)    

    call var_list_set_real (global%var_list, var_str ("sqrts"),&
         1000._default, is_known = .true.)

    call global%it_list%init ([1], [1000])

    call var_list_set_string (global%var_list, var_str ("$run_id"), &
         var_str ("simulations1"), is_known = .true.)
    call integrate_process (procname1, global)

    write (u, "(A)")  "* Initialize event generation"
    write (u, "(A)")

    call var_list_set_log (global%var_list, var_str ("?unweighted"), &
         .false., is_known = .true.)
    call simulation%init ([procname1], .true., .true., global)
    call simulation%init_process_selector ()

    data = simulation%get_data ()
    call data%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Generate three events"
    write (u, "(A)")

    call simulation%generate (3)
    call simulation%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Write the event record for the last event"
    write (u, "(A)")
    
    call simulation%write_event (u)

    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"

    call simulation%final ()
    call global%final ()
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: simulations_2"
    
  end subroutine simulations_2
  
  subroutine simulations_3 (u)
    integer, intent(in) :: u
    type(string_t) :: libname, procname1
    type(rt_data_t), target :: global
    type(simulation_t), target :: simulation
    type(event_sample_data_t) :: data
    
    write (u, "(A)")  "* Test output: simulations_3"
    write (u, "(A)")  "*   Purpose: generate unweighted events &
         &for a single process"
    write (u, "(A)")

    write (u, "(A)")  "* Initialize processes"
    write (u, "(A)")

    call syntax_model_file_init ()

    call global%global_init ()
    call var_list_set_log (global%var_list, var_str ("?omega_openmp"), &
         .false., is_known = .true.)
    call var_list_set_int (global%var_list, var_str ("seed"), &
         0, is_known = .true.)    
    
    libname = "simulation_3a"
    procname1 = "simulation_3p"
    
    call prepare_test_library (global, libname, 1, [procname1])
    call compile_library (libname, global)

    call var_list_append_log (global%var_list, &
         var_str ("?rebuild_events"), .true., intrinsic = .true.)

    call var_list_set_string (global%var_list, var_str ("$method"), &
         var_str ("unit_test"), is_known = .true.)
    call var_list_set_string (global%var_list, var_str ("$phs_method"), &
         var_str ("single"), is_known = .true.)
    call var_list_set_string (global%var_list, var_str ("$integration_method"),&
         var_str ("midpoint"), is_known = .true.)
    call var_list_set_log (global%var_list, var_str ("?vis_history"),&
         .false., is_known = .true.)    
    call var_list_set_log (global%var_list, var_str ("?integration_timer"),&
         .false., is_known = .true.)    

    call var_list_set_real (global%var_list, var_str ("sqrts"),&
         1000._default, is_known = .true.)

    call global%it_list%init ([1], [1000])

    call var_list_set_string (global%var_list, var_str ("$run_id"), &
         var_str ("simulations1"), is_known = .true.)
    call integrate_process (procname1, global)

    write (u, "(A)")  "* Initialize event generation"
    write (u, "(A)")

    call simulation%init ([procname1], .true., .true., global)
    call simulation%init_process_selector ()

    data = simulation%get_data ()
    call data%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Generate three events"
    write (u, "(A)")

    call simulation%generate (3)
    call simulation%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Write the event record for the last event"
    write (u, "(A)")
    
    call simulation%write_event (u)

    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"

    call simulation%final ()
    call global%final ()
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: simulations_3"
    
  end subroutine simulations_3
  
  subroutine simulations_4 (u)
    integer, intent(in) :: u
    type(string_t) :: libname, procname1
    type(rt_data_t), target :: global
    type(flavor_t) :: flv
    type(simulation_t), target :: simulation
    type(event_sample_data_t) :: data
    
    write (u, "(A)")  "* Test output: simulations_4"
    write (u, "(A)")  "*   Purpose: generate events for a single process &
         &with structure functions"
    write (u, "(A)")

    write (u, "(A)")  "* Initialize processes"
    write (u, "(A)")

    call syntax_model_file_init ()
    call syntax_phs_forest_init ()

    call global%global_init ()
    call var_list_set_log (global%var_list, var_str ("?omega_openmp"), &
         .false., is_known = .true.)
    call var_list_set_int (global%var_list, var_str ("seed"), &
         0, is_known = .true.)    
    
    libname = "simulation_4a"
    procname1 = "simulation_4p"
    
    call prepare_test_library (global, libname, 1, [procname1])
    call compile_library (libname, global)

    call var_list_append_log (global%var_list, &
         var_str ("?rebuild_phase_space"), .true., intrinsic = .true.)
    call var_list_append_log (global%var_list, &
         var_str ("?rebuild_grids"), .true., intrinsic = .true.)
    call var_list_append_log (global%var_list, &
         var_str ("?rebuild_events"), .true., intrinsic = .true.)

    call var_list_set_string (global%var_list, var_str ("$run_id"), &
         var_str ("r1"), is_known = .true.)
    call var_list_set_string (global%var_list, var_str ("$method"), &
         var_str ("unit_test"), is_known = .true.)
    call var_list_set_string (global%var_list, var_str ("$phs_method"), &
         var_str ("wood"), is_known = .true.)
    call var_list_set_string (global%var_list, var_str ("$integration_method"),&
         var_str ("vamp"), is_known = .true.)
    call var_list_set_log (global%var_list, var_str ("?use_vamp_equivalences"),&
         .true., is_known = .true.)
    call var_list_set_real (global%var_list, var_str ("sqrts"),&
         1000._default, is_known = .true.)
    call var_list_set_real (global%var_list, var_str ("ms"), &
         0._default, is_known = .true.)
    call var_list_set_log (global%var_list, var_str ("?vis_history"),&
         .false., is_known = .true.)    
    call var_list_set_log (global%var_list, var_str ("?integration_timer"),&
         .false., is_known = .true.)    
    
    call reset_interaction_counter ()

    call flavor_init (flv, 25, global%model)
    call global%beam_structure%init_sf (flavor_get_name ([flv, flv]), [1])
    call global%beam_structure%set_sf (1, 1, var_str ("sf_test_1"))

    write (u, "(A)")  "* Integrate"
    write (u, "(A)")

    call global%it_list%init ([1], [1000])

    call var_list_set_string (global%var_list, var_str ("$run_id"), &
         var_str ("r1"), is_known = .true.)
    call integrate_process (procname1, global)

    write (u, "(A)")  "* Initialize event generation"
    write (u, "(A)")

    call var_list_set_log (global%var_list, var_str ("?unweighted"), &
         .false., is_known = .true.)
    call var_list_set_string (global%var_list, var_str ("$sample"), &
         var_str ("simulations4"), is_known = .true.)
    call simulation%init ([procname1], .true., .true., global)
    call simulation%init_process_selector ()

    data = simulation%get_data ()
    call data%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Generate three events"
    write (u, "(A)")

    call simulation%generate (3)
    call simulation%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Write the event record for the last event"
    write (u, "(A)")
    
    call simulation%write_event (u)

    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"

    call simulation%final ()
    call global%final ()
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: simulations_4"
    
  end subroutine simulations_4
  
  subroutine simulations_5 (u)
    integer, intent(in) :: u
    type(string_t) :: libname, procname1, sample
    type(rt_data_t), target :: global
    type(process_ptr_t) :: process_ptr
    class(eio_t), allocatable :: eio
    type(simulation_t), allocatable, target :: simulation
    
    write (u, "(A)")  "* Test output: simulations_5"
    write (u, "(A)")  "*   Purpose: generate events for a single process"
    write (u, "(A)")  "*            write to file and reread"
    write (u, "(A)")

    write (u, "(A)")  "* Initialize processes"
    write (u, "(A)")

    call syntax_model_file_init ()

    call global%global_init ()
    call var_list_set_log (global%var_list, var_str ("?omega_openmp"), &
         .false., is_known = .true.)
    call var_list_set_int (global%var_list, var_str ("seed"), &
         0, is_known = .true.)    
    
    libname = "simulation_5a"
    procname1 = "simulation_5p"
    
    call prepare_test_library (global, libname, 1, [procname1])
    call compile_library (libname, global)

    call var_list_append_log (global%var_list, &
         var_str ("?rebuild_events"), .true., intrinsic = .true.)

    call var_list_set_string (global%var_list, var_str ("$method"), &
         var_str ("unit_test"), is_known = .true.)
    call var_list_set_string (global%var_list, var_str ("$phs_method"), &
         var_str ("single"), is_known = .true.)
    call var_list_set_string (global%var_list, var_str ("$integration_method"),&
         var_str ("midpoint"), is_known = .true.)   
    call var_list_set_log (global%var_list, var_str ("?vis_history"),&
         .false., is_known = .true.)    
    call var_list_set_log (global%var_list, var_str ("?integration_timer"),&
         .false., is_known = .true.)    

    call var_list_set_real (global%var_list, var_str ("sqrts"),&
         1000._default, is_known = .true.)

    call global%it_list%init ([1], [1000])

    call var_list_set_string (global%var_list, var_str ("$run_id"), &
         var_str ("simulations5"), is_known = .true.)
    call integrate_process (procname1, global)

    write (u, "(A)")  "* Initialize event generation"
    write (u, "(A)")

    call var_list_set_log (global%var_list, var_str ("?unweighted"), &
         .false., is_known = .true.)
    sample = "simulations5"
    call var_list_set_string (global%var_list, var_str ("$sample"), &
         sample, is_known = .true.)
    allocate (simulation)
    call simulation%init ([procname1], .true., .true., global)
    call simulation%init_process_selector ()

    write (u, "(A)")  "* Initialize raw event file"
    write (u, "(A)")

    process_ptr%ptr => global%process_stack%get_process_ptr (procname1)
    
    allocate (eio_raw_t :: eio)
    call eio%init_out (sample, [process_ptr])
    
    write (u, "(A)")  "* Generate an event"
    write (u, "(A)")

    call simulation%generate (1)
    call simulation%write_event (u)
    call simulation%write_event (eio)

    call eio%final ()
    deallocate (eio)
    call simulation%final ()
    deallocate (simulation)
    
    write (u, "(A)")
    write (u, "(A)")  "* Re-read the event from file"
    write (u, "(A)")
    
    allocate (simulation)
    call simulation%init ([procname1], .true., .true., global)
    call simulation%init_process_selector ()
    allocate (eio_raw_t :: eio)
    call eio%init_in (sample, [process_ptr])
    
    call simulation%read_event (eio)
    call simulation%write_event (u)

    write (u, "(A)")
    write (u, "(A)")  "* Recalculate process instance"
    write (u, "(A)")

    call simulation%recalculate (update_sqme = .true., update_weight = .true.)
    call simulation%entry(simulation%i_prc)%evaluate_expressions ()
    call simulation%write_event (u)

    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"

    call eio%final ()
    call simulation%final ()
    call global%final ()
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: simulations_5"
    
  end subroutine simulations_5
  
  subroutine simulations_6 (u)
    integer, intent(in) :: u
    type(string_t) :: libname, procname1, sample
    type(rt_data_t), target :: global
    type(process_ptr_t) :: process_ptr
    class(eio_t), allocatable :: eio
    type(simulation_t), allocatable, target :: simulation
    type(flavor_t) :: flv
    
    write (u, "(A)")  "* Test output: simulations_6"
    write (u, "(A)")  "*   Purpose: generate events for a single process"
    write (u, "(A)")  "*            write to file and reread"
    write (u, "(A)")

    write (u, "(A)")  "* Initialize process and integrate"
    write (u, "(A)")

    call syntax_model_file_init ()

    call global%global_init ()
    call var_list_set_log (global%var_list, var_str ("?omega_openmp"), &
         .false., is_known = .true.)
    call var_list_set_int (global%var_list, var_str ("seed"), &
         0, is_known = .true.)    
    
    libname = "simulation_6"
    procname1 = "simulation_6p"
    
    call prepare_test_library (global, libname, 1, [procname1])
    call compile_library (libname, global)

    call var_list_append_log (global%var_list, &
         var_str ("?rebuild_phase_space"), .true., intrinsic = .true.)
    call var_list_append_log (global%var_list, &
         var_str ("?rebuild_grids"), .true., intrinsic = .true.)
    call var_list_append_log (global%var_list, &
         var_str ("?rebuild_events"), .true., intrinsic = .true.)

    call var_list_set_string (global%var_list, var_str ("$method"), &
         var_str ("unit_test"), is_known = .true.)
    call var_list_set_string (global%var_list, var_str ("$phs_method"), &
         var_str ("wood"), is_known = .true.)
    call var_list_set_string (global%var_list, var_str ("$integration_method"),&
         var_str ("vamp"), is_known = .true.)
    call var_list_set_log (global%var_list, var_str ("?use_vamp_equivalences"),&
         .true., is_known = .true.)
    call var_list_set_log (global%var_list, var_str ("?vis_history"),&
         .false., is_known = .true.)    
    call var_list_set_log (global%var_list, var_str ("?integration_timer"),&
         .false., is_known = .true.)    

    call var_list_set_real (global%var_list, var_str ("sqrts"),&
         1000._default, is_known = .true.)
    call var_list_set_real (global%var_list, var_str ("ms"), &
         0._default, is_known = .true.)

    call flavor_init (flv, 25, global%model)
    call global%beam_structure%init_sf (flavor_get_name ([flv, flv]), [1])
    call global%beam_structure%set_sf (1, 1, var_str ("sf_test_1"))

    call global%it_list%init ([1], [1000])

    call var_list_set_string (global%var_list, var_str ("$run_id"), &
         var_str ("r1"), is_known = .true.)
    call integrate_process (procname1, global)

    write (u, "(A)")  "* Initialize event generation"
    write (u, "(A)")

    call reset_interaction_counter ()
    
    call var_list_set_log (global%var_list, var_str ("?unweighted"), &
         .false., is_known = .true.)
    sample = "simulations6"
    call var_list_set_string (global%var_list, var_str ("$sample"), &
         sample, is_known = .true.)
    allocate (simulation)
    call simulation%init ([procname1], .true., .true., global)
    call simulation%init_process_selector ()

    write (u, "(A)")  "* Initialize raw event file"
    write (u, "(A)")

    process_ptr%ptr => global%process_stack%get_process_ptr (procname1)
    
    allocate (eio_raw_t :: eio)
    call eio%init_out (sample, [process_ptr])
    
    write (u, "(A)")  "* Generate an event"
    write (u, "(A)")

    call simulation%generate (1)
    call pacify (simulation%entry(simulation%i_prc))
    call simulation%write_event (u, verbose = .true., testflag = .true.)
    call simulation%write_event (eio)

    call eio%final ()
    deallocate (eio)
    call simulation%final ()
    deallocate (simulation)
    
    write (u, "(A)")
    write (u, "(A)")  "* Re-read the event from file"
    write (u, "(A)")
    
    call reset_interaction_counter ()
    
    allocate (simulation)
    call simulation%init ([procname1], .true., .true., global)
    call simulation%init_process_selector ()
    allocate (eio_raw_t :: eio)
    call eio%init_in (sample, [process_ptr])
    
    call simulation%read_event (eio)
    call simulation%write_event (u, verbose = .true., testflag = .true.)

    write (u, "(A)")
    write (u, "(A)")  "* Recalculate process instance"
    write (u, "(A)")

    call simulation%recalculate (update_sqme = .true., update_weight = .true.)
    call simulation%entry(simulation%i_prc)%evaluate_expressions ()
    call simulation%write_event (u, verbose = .true., testflag = .true.)

    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"

    call eio%final ()
    call simulation%final ()
    call global%final ()
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: simulations_6"
    
  end subroutine simulations_6
  
  subroutine simulations_7 (u)
    integer, intent(in) :: u
    type(string_t) :: libname, procname1, sample
    type(rt_data_t), target :: global
    type(string_t), dimension(0) :: empty_string_array
    type(event_sample_data_t) :: data
    type(event_stream_array_t) :: es_array
    type(simulation_t), allocatable, target :: simulation
    type(flavor_t) :: flv
    
    write (u, "(A)")  "* Test output: simulations_7"
    write (u, "(A)")  "*   Purpose: generate events for a single process"
    write (u, "(A)")  "*            write to file and reread"
    write (u, "(A)")

    write (u, "(A)")  "* Initialize process and integrate"
    write (u, "(A)")

    call syntax_model_file_init ()

    call global%global_init ()
    call var_list_set_log (global%var_list, var_str ("?omega_openmp"), &
         .false., is_known = .true.)
    call var_list_set_int (global%var_list, var_str ("seed"), &
         0, is_known = .true.)    
    
    libname = "simulation_7"
    procname1 = "simulation_7p"
    
    call prepare_test_library (global, libname, 1, [procname1])
    call compile_library (libname, global)

    call var_list_append_log (global%var_list, &
         var_str ("?rebuild_phase_space"), .true., intrinsic = .true.)
    call var_list_append_log (global%var_list, &
         var_str ("?rebuild_grids"), .true., intrinsic = .true.)
    call var_list_append_log (global%var_list, &
         var_str ("?rebuild_events"), .true., intrinsic = .true.)

    call var_list_set_string (global%var_list, var_str ("$method"), &
         var_str ("unit_test"), is_known = .true.)
    call var_list_set_string (global%var_list, var_str ("$phs_method"), &
         var_str ("wood"), is_known = .true.)
    call var_list_set_string (global%var_list, var_str ("$integration_method"),&
         var_str ("vamp"), is_known = .true.)
    call var_list_set_log (global%var_list, var_str ("?use_vamp_equivalences"),&
         .true., is_known = .true.)
    call var_list_set_log (global%var_list, var_str ("?vis_history"),&
         .false., is_known = .true.)    
    call var_list_set_log (global%var_list, var_str ("?integration_timer"),&
         .false., is_known = .true.)    

    call var_list_set_real (global%var_list, var_str ("sqrts"),&
         1000._default, is_known = .true.)
    call var_list_set_real (global%var_list, var_str ("ms"), &
         0._default, is_known = .true.)

    call flavor_init (flv, 25, global%model)
    call global%beam_structure%init_sf (flavor_get_name ([flv, flv]), [1])
    call global%beam_structure%set_sf (1, 1, var_str ("sf_test_1"))

    call global%it_list%init ([1], [1000])

    call var_list_set_string (global%var_list, var_str ("$run_id"), &
         var_str ("r1"), is_known = .true.)
    call integrate_process (procname1, global)

    write (u, "(A)")  "* Initialize event generation"
    write (u, "(A)")

    call reset_interaction_counter ()
    
    call var_list_set_log (global%var_list, var_str ("?unweighted"), &
         .false., is_known = .true.)
    sample = "simulations7"
    call var_list_set_string (global%var_list, var_str ("$sample"), &
         sample, is_known = .true.)
    allocate (simulation)
    call simulation%init ([procname1], .true., .true., global)
    call simulation%init_process_selector ()

    write (u, "(A)")  "* Initialize raw event file"
    write (u, "(A)")

    data%md5sum_prc = simulation%get_md5sum_prc ()
    data%md5sum_cfg = simulation%get_md5sum_cfg ()
    call es_array%init &
         (sample, [var_str ("raw")], simulation%get_process_ptr (), global, &
         data)
    
    write (u, "(A)")  "* Generate an event"
    write (u, "(A)")

    call simulation%generate (1, es_array)

    call es_array%final ()
    call simulation%final ()
    deallocate (simulation)
    
    write (u, "(A)")  "* Re-read the event from file and generate another one"
    write (u, "(A)")
    
    call var_list_set_log (global%var_list, &
         var_str ("?rebuild_events"), .false., is_known = .true.)

    call reset_interaction_counter ()
    
    allocate (simulation)
    call simulation%init ([procname1], .true., .true., global)
    call simulation%init_process_selector ()

    data%md5sum_prc = simulation%get_md5sum_prc ()
    data%md5sum_cfg = simulation%get_md5sum_cfg ()
    call es_array%init (sample, &
         empty_string_array, simulation%get_process_ptr (), global, data, &
         input = var_str ("raw"))
    
    call simulation%generate (2, es_array)
    
    call pacify (simulation%entry(simulation%i_prc))
    call simulation%write_event (u, verbose = .true.)

    call es_array%final ()
    call simulation%final ()
    deallocate (simulation)
    
    
    write (u, "(A)")
    write (u, "(A)")  "* Re-read both events from file"
    write (u, "(A)")
    
    call reset_interaction_counter ()
    
    allocate (simulation)
    call simulation%init ([procname1], .true., .true., global)
    call simulation%init_process_selector ()

    data%md5sum_prc = simulation%get_md5sum_prc ()
    data%md5sum_cfg = simulation%get_md5sum_cfg ()
    call es_array%init (sample, &
         empty_string_array, simulation%get_process_ptr (), global, data, &
         input = var_str ("raw"))

    call simulation%generate (2, es_array)
    
    call pacify (simulation%entry(simulation%i_prc))
    call simulation%write_event (u, verbose = .true.)

    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"

    call es_array%final ()
    call simulation%final ()
    call global%final ()
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: simulations_7"
    
  end subroutine simulations_7
  
  subroutine simulations_8 (u)
    integer, intent(in) :: u
    type(string_t) :: libname, procname1, sample
    type(rt_data_t), target :: global
    type(string_t), dimension(0) :: empty_string_array
    type(event_sample_data_t) :: data
    type(event_stream_array_t) :: es_array
    type(simulation_t), allocatable, target :: simulation
    type(flavor_t) :: flv
    
    write (u, "(A)")  "* Test output: simulations_8"
    write (u, "(A)")  "*   Purpose: generate events for a single process"
    write (u, "(A)")  "*            write to file and rescan"
    write (u, "(A)")

    write (u, "(A)")  "* Initialize process and integrate"
    write (u, "(A)")

    call syntax_model_file_init ()

    call global%global_init ()
    call var_list_set_log (global%var_list, var_str ("?omega_openmp"), &
         .false., is_known = .true.)
    call var_list_set_int (global%var_list, var_str ("seed"), &
         0, is_known = .true.)        

    libname = "simulation_8"
    procname1 = "simulation_8p"
    
    call prepare_test_library (global, libname, 1, [procname1])
    call compile_library (libname, global)

    call var_list_append_log (global%var_list, &
         var_str ("?rebuild_phase_space"), .true., intrinsic = .true.)
    call var_list_append_log (global%var_list, &
         var_str ("?rebuild_grids"), .true., intrinsic = .true.)
    call var_list_append_log (global%var_list, &
         var_str ("?rebuild_events"), .true., intrinsic = .true.)

    call var_list_set_string (global%var_list, var_str ("$method"), &
         var_str ("unit_test"), is_known = .true.)
    call var_list_set_string (global%var_list, var_str ("$phs_method"), &
         var_str ("wood"), is_known = .true.)
    call var_list_set_string (global%var_list, var_str ("$integration_method"),&
         var_str ("vamp"), is_known = .true.)
    call var_list_set_log (global%var_list, var_str ("?use_vamp_equivalences"),&
         .true., is_known = .true.)   
    call var_list_set_log (global%var_list, var_str ("?vis_history"),&
         .false., is_known = .true.)    
    call var_list_set_log (global%var_list, var_str ("?integration_timer"),&
         .false., is_known = .true.)    

    call var_list_set_real (global%var_list, var_str ("sqrts"),&
         1000._default, is_known = .true.)
    call var_list_set_real (global%var_list, var_str ("ms"), &
         0._default, is_known = .true.)

    call flavor_init (flv, 25, global%model)
    call global%beam_structure%init_sf (flavor_get_name ([flv, flv]), [1])
    call global%beam_structure%set_sf (1, 1, var_str ("sf_test_1"))

    call global%it_list%init ([1], [1000])

    call var_list_set_string (global%var_list, var_str ("$run_id"), &
         var_str ("r1"), is_known = .true.)
    call integrate_process (procname1, global)

    write (u, "(A)")  "* Initialize event generation"
    write (u, "(A)")

    call reset_interaction_counter ()
    
    call var_list_set_log (global%var_list, var_str ("?unweighted"), &
         .false., is_known = .true.)
    sample = "simulations8"
    call var_list_set_string (global%var_list, var_str ("$sample"), &
         sample, is_known = .true.)
    allocate (simulation)
    call simulation%init ([procname1], .true., .true., global)
    call simulation%init_process_selector ()

    write (u, "(A)")  "* Initialize raw event file"
    write (u, "(A)")

    data%md5sum_prc = simulation%get_md5sum_prc ()
    data%md5sum_cfg = simulation%get_md5sum_cfg ()
    write (u, "(1x,A,A,A)")  "MD5 sum (proc)   = '", data%md5sum_prc, "'"
    write (u, "(1x,A,A,A)")  "MD5 sum (config) = '", data%md5sum_cfg, "'"
    call es_array%init &
         (sample, [var_str ("raw")], simulation%get_process_ptr (), global, &
         data)
    
    write (u, "(A)")
    write (u, "(A)")  "* Generate an event"
    write (u, "(A)")

    call simulation%generate (1, es_array)

    call pacify (simulation%entry(simulation%i_prc))
    call simulation%write_event (u, verbose = .true., testflag = .true.)

    call es_array%final ()
    call simulation%final ()
    deallocate (simulation)
    
    write (u, "(A)")
    write (u, "(A)")  "* Re-read the event from file"
    write (u, "(A)")
    
    call reset_interaction_counter ()
    
    allocate (simulation)
    call simulation%init ([procname1], .false., .false., global)
    call simulation%init_process_selector ()

    data%md5sum_prc = simulation%get_md5sum_prc ()
    data%md5sum_cfg = ""
    write (u, "(1x,A,A,A)")  "MD5 sum (proc)   = '", data%md5sum_prc, "'"
    write (u, "(1x,A,A,A)")  "MD5 sum (config) = '", data%md5sum_cfg, "'"
    call es_array%init (sample, &
         empty_string_array, simulation%get_process_ptr (), global, data, &
         input = var_str ("raw"), input_sample = sample, allow_switch = .false.)
    
    call simulation%rescan (1, es_array, &
         update_event = .false., &
         update_sqme = .false., &
         update_weight = .false., &
         recover_beams = .false., &
         global = global)
    
    write (u, "(A)")

    call pacify (simulation%entry(simulation%i_prc))
    call simulation%write_event (u, verbose = .true., testflag = .true.)

    call es_array%final ()
    call simulation%final ()
    deallocate (simulation)
    
    write (u, "(A)")
    write (u, "(A)")  "* Re-read again and recalculate"
    write (u, "(A)")
    
    call reset_interaction_counter ()
    
    allocate (simulation)
    call simulation%init ([procname1], .false., .false., global)
    call simulation%init_process_selector ()

    data%md5sum_prc = simulation%get_md5sum_prc ()
    data%md5sum_cfg = ""
    write (u, "(1x,A,A,A)")  "MD5 sum (proc)   = '", data%md5sum_prc, "'"
    write (u, "(1x,A,A,A)")  "MD5 sum (config) = '", data%md5sum_cfg, "'"
    call es_array%init (sample, &
         empty_string_array, simulation%get_process_ptr (), global, data, &
         input = var_str ("raw"), input_sample = sample, allow_switch = .false.)
    
    call simulation%rescan (1, es_array, &
         update_event = .true., &
         update_sqme = .true., &
         update_weight = .false., &
         recover_beams = .false., &
         global = global)
    
    write (u, "(A)")

    call pacify (simulation%entry(simulation%i_prc))
    call simulation%write_event (u, verbose = .true., testflag = .true.)

    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"

    call es_array%final ()
    call simulation%final ()
    call global%final ()
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: simulations_8"
    
  end subroutine simulations_8
  
  subroutine simulations_9 (u)
    integer, intent(in) :: u
    type(string_t) :: libname, procname1, sample
    type(rt_data_t), target :: global
    type(string_t), dimension(0) :: empty_string_array
    type(event_sample_data_t) :: data
    type(event_stream_array_t) :: es_array
    type(simulation_t), allocatable, target :: simulation
    type(flavor_t) :: flv
    logical :: error
    
    write (u, "(A)")  "* Test output: simulations_9"
    write (u, "(A)")  "*   Purpose: generate events for a single process"
    write (u, "(A)")  "*            write to file and rescan"
    write (u, "(A)")

    write (u, "(A)")  "* Initialize process and integrate"
    write (u, "(A)")

    call syntax_model_file_init ()

    call global%global_init ()
    call var_list_set_log (global%var_list, var_str ("?omega_openmp"), &
         .false., is_known = .true.)
    call var_list_set_int (global%var_list, var_str ("seed"), &
         0, is_known = .true.)    
    
    libname = "simulation_9"
    procname1 = "simulation_9p"
    
    call prepare_test_library (global, libname, 1, [procname1])
    call compile_library (libname, global)

    call var_list_append_log (global%var_list, &
         var_str ("?rebuild_phase_space"), .true., intrinsic = .true.)
    call var_list_append_log (global%var_list, &
         var_str ("?rebuild_grids"), .true., intrinsic = .true.)
    call var_list_append_log (global%var_list, &
         var_str ("?rebuild_events"), .true., intrinsic = .true.)

    call var_list_set_string (global%var_list, var_str ("$method"), &
         var_str ("unit_test"), is_known = .true.)
    call var_list_set_string (global%var_list, var_str ("$phs_method"), &
         var_str ("wood"), is_known = .true.)
    call var_list_set_string (global%var_list, var_str ("$integration_method"),&
         var_str ("vamp"), is_known = .true.)
    call var_list_set_log (global%var_list, var_str ("?use_vamp_equivalences"),&
         .true., is_known = .true.)
    call var_list_set_log (global%var_list, var_str ("?vis_history"),&
         .false., is_known = .true.)    
    call var_list_set_log (global%var_list, var_str ("?integration_timer"),&
         .false., is_known = .true.)    

    call var_list_set_real (global%var_list, var_str ("sqrts"),&
         1000._default, is_known = .true.)
    call var_list_set_real (global%var_list, var_str ("ms"), &
         0._default, is_known = .true.)

    call flavor_init (flv, 25, global%model)
    call global%beam_structure%init_sf (flavor_get_name ([flv, flv]), [1])
    call global%beam_structure%set_sf (1, 1, var_str ("sf_test_1"))

    call global%it_list%init ([1], [1000])

    call var_list_set_string (global%var_list, var_str ("$run_id"), &
         var_str ("r1"), is_known = .true.)
    call integrate_process (procname1, global)

    write (u, "(A)")  "* Initialize event generation"
    write (u, "(A)")

    call reset_interaction_counter ()
    
    call var_list_set_log (global%var_list, var_str ("?unweighted"), &
         .false., is_known = .true.)
    sample = "simulations9"
    call var_list_set_string (global%var_list, var_str ("$sample"), &
         sample, is_known = .true.)
    allocate (simulation)
    call simulation%init ([procname1], .true., .true., global)
    call simulation%init_process_selector ()

    call simulation%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Initialize raw event file"
    write (u, "(A)")

    data%md5sum_prc = simulation%get_md5sum_prc ()
    data%md5sum_cfg = simulation%get_md5sum_cfg ()
    write (u, "(1x,A,A,A)")  "MD5 sum (proc)   = '", data%md5sum_prc, "'"
    write (u, "(1x,A,A,A)")  "MD5 sum (config) = '", data%md5sum_cfg, "'"
    call es_array%init &
         (sample, [var_str ("raw")], simulation%get_process_ptr (), global, &
         data)
    
    write (u, "(A)")
    write (u, "(A)")  "* Generate an event"
    write (u, "(A)")

    call simulation%generate (1, es_array)

    call es_array%final ()
    call simulation%final ()
    deallocate (simulation)
    
    write (u, "(A)")  "* Initialize event generation for different parameters"
    write (u, "(A)")
    
    call reset_interaction_counter ()
    
    allocate (simulation)
    call simulation%init ([procname1, procname1], .false., .false., global)
    call simulation%init_process_selector ()
    
    call simulation%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Attempt to re-read the events (should fail)"
    write (u, "(A)")

    data%md5sum_prc = simulation%get_md5sum_prc ()
    data%md5sum_cfg = ""
    write (u, "(1x,A,A,A)")  "MD5 sum (proc)   = '", data%md5sum_prc, "'"
    write (u, "(1x,A,A,A)")  "MD5 sum (config) = '", data%md5sum_cfg, "'"
    call es_array%init (sample, &
         empty_string_array, simulation%get_process_ptr (), global, data, &
         input = var_str ("raw"), input_sample = sample, &
         allow_switch = .false., error = error)
    
    write (u, "(1x,A,L1)")  "error = ", error
    
    call simulation%rescan (1, es_array, &
         update_event = .false., &
         update_sqme = .false., &
         update_weight = .false., &
         recover_beams = .false., &
         global = global)

    call es_array%final ()
    call simulation%final ()
    call global%final ()
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: simulations_9"
    
  end subroutine simulations_9
  
  subroutine simulations_10 (u)
    integer, intent(in) :: u
    type(string_t) :: libname, procname1, expr_text
    type(rt_data_t), target :: global
    type(rt_data_t), dimension(1), target :: alt_env
    type(ifile_t) :: ifile
    type(stream_t) :: stream
    type(parse_tree_t) :: pt_weight
    type(simulation_t), target :: simulation
    type(event_sample_data_t) :: data
    
    write (u, "(A)")  "* Test output: simulations_10"
    write (u, "(A)")  "*   Purpose: reweight event"
    write (u, "(A)")

    write (u, "(A)")  "* Initialize processes"
    write (u, "(A)")

    call syntax_model_file_init ()
    call syntax_pexpr_init ()

    call global%global_init ()
    call var_list_set_log (global%var_list, var_str ("?omega_openmp"), &
         .false., is_known = .true.)
    call var_list_set_int (global%var_list, var_str ("seed"), &
         0, is_known = .true.)    
    
    libname = "simulation_10a"
    procname1 = "simulation_10p"
    
    call prepare_test_library (global, libname, 1, [procname1])
    call compile_library (libname, global)

    call var_list_append_log (global%var_list, &
         var_str ("?rebuild_phase_space"), .true., intrinsic = .true.)
    call var_list_append_log (global%var_list, &
         var_str ("?rebuild_grids"), .true., intrinsic = .true.)
    call var_list_append_log (global%var_list, &
         var_str ("?rebuild_events"), .true., intrinsic = .true.)

    call var_list_set_string (global%var_list, var_str ("$method"), &
         var_str ("unit_test"), is_known = .true.)
    call var_list_set_string (global%var_list, var_str ("$phs_method"), &
         var_str ("single"), is_known = .true.)
    call var_list_set_string (global%var_list, var_str ("$integration_method"),&
         var_str ("midpoint"), is_known = .true.)
    call var_list_set_log (global%var_list, var_str ("?vis_history"),&
         .false., is_known = .true.)    
    call var_list_set_log (global%var_list, var_str ("?integration_timer"),&
         .false., is_known = .true.)    

    call var_list_set_real (global%var_list, var_str ("sqrts"),&
         1000._default, is_known = .true.)

    call global%it_list%init ([1], [1000])

    call var_list_set_string (global%var_list, var_str ("$run_id"), &
         var_str ("simulations1"), is_known = .true.)
    call integrate_process (procname1, global)

    write (u, "(A)")  "* Initialize alternative environment with custom weight"
    write (u, "(A)")
    
    call alt_env(1)%local_init (global)
    call alt_env(1)%link (global)

    expr_text = "2"
    write (u, "(A,A)")  "weight = ", char (expr_text)
    write (u, *)
    
    call ifile_clear (ifile)
    call ifile_append (ifile, expr_text)
    call stream_init (stream, ifile)
    call parse_tree_init_expr (pt_weight, stream, .true.)
    call stream_final (stream)
    alt_env(1)%pn%weight_expr => parse_tree_get_root_ptr (pt_weight)
    call alt_env(1)%write_expr (u)

    write (u, "(A)")
    write (u, "(A)")  "* Initialize event generation"
    write (u, "(A)")

    call var_list_set_log (global%var_list, var_str ("?unweighted"), &
         .false., is_known = .true.)
    call simulation%init ([procname1], .true., .true., global, alt_env)
    call simulation%init_process_selector ()

    data = simulation%get_data ()
    call data%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Generate an event"
    write (u, "(A)")

    call simulation%generate (1)
    call simulation%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Write the event record for the last event"
    write (u, "(A)")
    
    call simulation%write_event (u)

    write (u, "(A)")
    write (u, "(A)")  "* Write the event record for the alternative setup"
    write (u, "(A)")
    
    call simulation%write_alt_event (u)

    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"

    call simulation%final ()
    call global%final ()
    
    call syntax_model_file_final ()
    call syntax_pexpr_final ()

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: simulations_10"
    
  end subroutine simulations_10
  
  subroutine simulations_11 (u)
    integer, intent(in) :: u
    type(rt_data_t), target :: global
    type(prclib_entry_t), pointer :: lib
    type(string_t) :: prefix, procname1, procname2
    type(simulation_t), target :: simulation
    
    write (u, "(A)")  "* Test output: simulations_11"
    write (u, "(A)")  "*   Purpose: apply decay"
    write (u, "(A)")

    write (u, "(A)")  "* Initialize processes"
    write (u, "(A)")

    call syntax_model_file_init ()
        
    call global%global_init ()
    allocate (lib)
    call global%add_prclib (lib)

    call var_list_set_int (global%var_list, var_str ("seed"), &
         0, is_known = .true.)        
    
    prefix = "simulation_11"
    procname1 = prefix // "_p"
    procname2 = prefix // "_d"
    call prepare_testbed &
         (global%prclib, global%process_stack, global%model_list, &
         global%model, prefix, global%os_data, &
         scattering=.true., decay=.true.)
    call model_set_unstable (global%model, 25, [procname2])

    write (u, "(A)")  "* Initialize simulation object"
    write (u, "(A)")

    call simulation%init ([procname1], .true., .true., global)
    call simulation%init_process_selector ()

    write (u, "(A)")  "* Generate event"
    write (u, "(A)")

    call simulation%generate (1)
    call simulation%write (u)

    write (u, *)
    
    call simulation%write_event (u)

    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"
    write (u, "(A)")

    call simulation%final ()
    call global%final ()
    
    call syntax_model_file_final ()

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: simulations_11"
    
  end subroutine simulations_11
  
  subroutine simulations_12 (u)
    integer, intent(in) :: u
    type(string_t) :: libname, procname1, sample
    type(rt_data_t), target :: global
    type(process_ptr_t) :: process_ptr
    class(eio_t), allocatable :: eio
    type(simulation_t), allocatable, target :: simulation
    type(flavor_t) :: flv
    integer :: i_evt
    
    write (u, "(A)")  "* Test output: simulations_12"
    write (u, "(A)")  "*   Purpose: generate events for a single process"
    write (u, "(A)")  "*            and write to split event files"
    write (u, "(A)")

    write (u, "(A)")  "* Initialize process and integrate"
    write (u, "(A)")

    call syntax_model_file_init ()

    call global%global_init ()
    call var_list_set_log (global%var_list, var_str ("?omega_openmp"), &
         .false., is_known = .true.)
    call var_list_set_int (global%var_list, var_str ("seed"), &
         0, is_known = .true.)    
    
    libname = "simulation_12"
    procname1 = "simulation_12p"
    
    call prepare_test_library (global, libname, 1, [procname1])
    call compile_library (libname, global)

    call var_list_append_log (global%var_list, &
         var_str ("?rebuild_phase_space"), .true., intrinsic = .true.)
    call var_list_append_log (global%var_list, &
         var_str ("?rebuild_grids"), .true., intrinsic = .true.)
    call var_list_append_log (global%var_list, &
         var_str ("?rebuild_events"), .true., intrinsic = .true.)

    call var_list_set_string (global%var_list, var_str ("$method"), &
         var_str ("unit_test"), is_known = .true.)
    call var_list_set_string (global%var_list, var_str ("$phs_method"), &
         var_str ("single"), is_known = .true.)
    call var_list_set_string (global%var_list, var_str ("$integration_method"),&
         var_str ("midpoint"), is_known = .true.)
    call var_list_set_log (global%var_list, var_str ("?vis_history"),&
         .false., is_known = .true.)    
    call var_list_set_log (global%var_list, var_str ("?integration_timer"),&
         .false., is_known = .true.)    

    call var_list_set_real (global%var_list, var_str ("sqrts"),&
         1000._default, is_known = .true.)
    call var_list_set_real (global%var_list, var_str ("ms"), &
         0._default, is_known = .true.)

    call flavor_init (flv, 25, global%model)

    call global%it_list%init ([1], [1000])

    call var_list_set_string (global%var_list, var_str ("$run_id"), &
         var_str ("r1"), is_known = .true.)
    call integrate_process (procname1, global)

    write (u, "(A)")  "* Initialize event generation"
    write (u, "(A)")

    call var_list_set_log (global%var_list, var_str ("?unweighted"), &
         .false., is_known = .true.)
    sample = "simulations_12"
    call var_list_set_string (global%var_list, var_str ("$sample"), &
         sample, is_known = .true.)
    call var_list_set_int (global%var_list, var_str ("sample_split_n_evt"), &
         2, is_known = .true.)
    call var_list_set_int (global%var_list, var_str ("sample_split_index"), &
         42, is_known = .true.)
    allocate (simulation)
    call simulation%init ([procname1], .true., .true., global)
    call simulation%init_process_selector ()

    call simulation%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Initialize ASCII event file"
    write (u, "(A)")

    process_ptr%ptr => global%process_stack%get_process_ptr (procname1)
    
    allocate (eio_ascii_short_t :: eio)
    select type (eio)
    class is (eio_ascii_t);  call eio%set_parameters ()
    end select
    call eio%init_out (sample, [process_ptr], data = simulation%get_data ())
    
    write (u, "(A)")  "* Generate 5 events, distributed among three files"

    do i_evt = 1, 5
       call simulation%generate (1)
       call simulation%write_event (eio)
    end do

    call eio%final ()
    deallocate (eio)
    call simulation%final ()
    deallocate (simulation)
    
    write (u, *)
    call display_file ("simulations_12.42.short.evt", u)
    write (u, *)
    call display_file ("simulations_12.43.short.evt", u)
    write (u, *)
    call display_file ("simulations_12.44.short.evt", u)

    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"

    call global%final ()
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: simulations_12"
    
  end subroutine simulations_12
  
  subroutine display_file (file, u)
    character(*), intent(in) :: file
    integer, intent(in) :: u
    character(256) :: buffer
    integer :: u_file
    write (u, "(3A)")  "* Contents of file '", file, "':"
    write (u, *)
    u_file = free_unit ()
    open (u_file, file = file, action = "read", status = "old")
    do
       read (u_file, "(A)", end = 1)  buffer
       write (u, "(A)")  trim (buffer)
    end do
1   continue
  end subroutine display_file


end module simulations
