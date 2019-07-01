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

module processes

  use kinds, only: default !NODEP!
  use iso_varying_string, string_t => varying_string !NODEP!
  use limits, only: GML_MIN_RANGE_RATIO !NODEP!
  use system_dependencies !NODEP!
  use constants !NODEP!
  use file_utils !NODEP!
  use diagnostics !NODEP!
  use sm_physics !NODEP!
  use vamp_equivalences !NODEP!
  use vamp !NODEP!
  use tao_random_numbers !NODEP!
  use md5
  use cputime
  use os_interface
  use lexers
  use parser
  use lorentz !NODEP!
  use subevents
  use variables
  use expressions
  use models
  use flavors
  use quantum_numbers
  use polarizations
  use interactions
  use evaluators
  use particles
  use beams
  use sf_isr
  use sf_epa
  use sf_ewa
  use sf_circe1
  use sf_circe2
  use sf_escan
  use sf_beam_events
  use sf_lhapdf
  use sf_pdf_builtin
  use sf_user
  use strfun
  use mappings
  use phs_forests
  use cascades
  use process_libraries
  use prclib_interfaces
  use hard_interactions

  implicit none
  private

  public :: integration_results_t
  public :: integration_results_append
  public :: process_status_t
  public :: process_status_write_counters
  public :: process_t
  public :: process_assign_global_var_list
  public :: process_write
  public :: process_write_logfile
  public :: process_display_integration_history
  public :: process_p
  public :: process_ptr_array_create
  public :: process_is_valid
  public :: process_has_matrix_element
  public :: process_has_integral
  public :: process_uses_beams
  public :: process_get_id
  public :: process_get_lib_index
  public :: process_get_store_index
  public :: process_get_md5sum
  public :: process_get_md5sum_parameters
  public :: process_get_md5sum_results
  public :: process_get_md5sum_polarized
  public :: process_get_model_ptr
  public :: process_get_n_in
  public :: process_get_n_out
  public :: process_get_beam_index
  public :: process_get_incoming_parton_index
  public :: process_get_outgoing_parton_index
  public :: process_get_beam_flv
  public :: process_get_beam_energy
  public :: process_get_n_parameters
  public :: process_get_n_channels
  public :: process_get_n_bins
  public :: process_get_status
  public :: process_get_scale
  public :: process_get_fac_scale
  public :: process_get_ren_scale  
  public :: process_get_alpha_s
  public :: process_get_sqme
  public :: process_get_reweighting_factor
  public :: process_get_n_calls
  public :: process_get_integral 
  public :: process_get_error
  public :: process_get_accuracy
  public :: process_get_chi2
  public :: process_get_time_per_event
  public :: process_get_efficiency
  public :: process_get_sample_function_value
  public :: process_get_current_pass
  public :: process_get_current_it
  public :: process_get_eval_sqme_ptr
  public :: process_get_eval_flows_ptr
  public :: process_get_hi_int_ptr
  public :: process_get_hi_eval_sqme_ptr
  public :: process_get_hi_eval_flows_ptr
  public :: process_mark_as_cascade_decay
  public :: process_set_scale
  public :: process_set_fac_scale
  public :: process_set_ren_scale  
  public :: process_set_alpha_s
  public :: process_set_sqme
  public :: process_setup_beams
  public :: process_set_beam_momenta
  public :: process_set_strfun
  public :: process_set_strfun_mapping
  public :: process_allow_global_mapping
  public :: process_connect_strfun
  public :: process_check_beam_setup
  public :: process_setup_phase_space
  public :: process_setup_subevt
  public :: process_setup_cuts
  public :: process_setup_weight
  public :: process_setup_scale
  public :: process_setup_fac_scale
  public :: process_setup_ren_scale
  public :: grid_parameters_t
  public :: process_setup_grids
  public :: process_reset_helicity_selection
  public :: process_complete_kinematics
  public :: process_recover_kinematics
  public :: process_fill_subevt
  public :: process_compute_reweighting_factor
  public :: process_compute_scale
  public :: process_update_parameters
  public :: process_update_alpha_s
  public :: process_evaluate
  public :: process_integrate
  public :: process_do_dummy_integration
  public :: process_me_test
  public :: process_init_vamp_history
  public :: process_final_vamp_history
  public :: process_write_time_estimate
  public :: md5sum_grids_t
  public :: process_read_grid_file
  public :: process_setup_event_generation
  public :: process_generate_weighted_event
  public :: process_generate_unweighted_event
  public :: process_complete_evaluators
  public :: process_get_unstable_products
  public :: process_set_particles
  public :: process_results_write_header
  public :: process_results_write_entry
  public :: process_results_write_current
  public :: process_results_write_average
  public :: process_results_write_current_average
  public :: process_results_write_footer
  public :: process_results_write
  public :: process_record_integral
  public :: process_request_copy
  public :: process_tag_as_working_copy
  public :: process_free_copy
  public :: process_store_final
  public :: process_store_unload
  public :: process_store_reload
  public :: process_store_write
  public :: process_store_write_results
  public :: process_store_get_process_ptr
  public :: process_store_init_process
  public :: process_test

  integer, parameter :: PRC_UNKNOWN = 0
  integer, parameter :: PRC_DECAY = 1
  integer, parameter :: PRC_SCATTERING = 2

  integer, parameter :: RESULTS_CHUNK_SIZE = 10


  type :: integration_entry_t
     private
     integer :: process_type = PRC_UNKNOWN
     integer :: pass = 0
     integer :: it = 0
     integer :: n_it = 0
     integer :: n_calls = 0
     logical :: improved = .false.
     real(default) :: integral = 0
     real(default) :: error = 0
     real(default) :: efficiency = 0
     real(default) :: chi2 = 0
     real(default), dimension(:), allocatable :: grove_weight
     type(time_t) :: time_start
     type(time_t) :: time_end
  end type integration_entry_t

  type :: integration_results_t
     private
     integer :: n_pass = 0
     integer :: n_it = 0
     type(integration_entry_t), dimension(:), allocatable :: entry
     type(integration_entry_t), dimension(:), allocatable :: average
  end type integration_results_t

  type :: process_status_t
     logical :: called = .false.
     logical :: passed_strfun_chain = .false.
     logical :: passed_mass_threshold = .false.
     logical :: passed_kinematics = .false.
     logical :: passed_cuts = .false.
     logical :: passed_evaluation = .false.
     integer :: n_called = 0
     integer :: n_passed_strfun_chain = 0
     integer :: n_passed_mass_threshold = 0
     integer :: n_passed_kinematics = 0
     integer :: n_passed_cuts = 0
     integer :: n_passed_evaluation = 0
  end type process_status_t

  type :: qcd_parameters_t
     logical :: alpha_s_is_fixed = .true.
     integer :: order = 0
     integer :: nf = 0
     logical :: alpha_s_from_mz = .true.
     logical :: mz_is_known = .false.
     real(default) :: mz = 0
     logical :: alpha_s_mz_is_known = .false.
     real(default) :: alpha_s_mz = 0
     real(default) :: lambda = 0
     real(default) :: alpha_s_at_scale = 0
     logical :: alpha_s_from_lhapdf = .false.
     integer :: lhapdf_set = 0
     integer :: lhapdf_member = 0
  end type qcd_parameters_t

  type :: process_t
     private
     integer :: type = PRC_UNKNOWN
     type(process_t), pointer :: copy => null ()
     logical :: is_original = .true.
     type(process_t), pointer :: original => null ()
     type(process_t), pointer :: working_copy => null ()
     logical :: in_use = .true.
     logical :: initialized = .false.
     logical :: has_matrix_element = .false.
     logical :: use_hi_color_factors = .false.
     logical :: use_beams = .true.
     logical :: has_extra_evaluators = .true.
     logical :: beams_are_set = .false.
     logical :: is_cascade_decay = .false.
     type(flavor_t), dimension(:), allocatable :: flv_in
     type(flavor_t), dimension(:), allocatable :: flv_out
     type(beam_data_t) :: beam_data
     type(string_t) :: id
     character(32) :: md5sum = ""
     type(process_library_t), pointer :: prc_lib => null ()
     integer :: lib_index = 0
     integer :: store_index = 0
     type(model_t), pointer :: model
     integer :: n_strfun = 0
     integer :: n_par_strfun = 0
     integer :: n_par_hi = 0
     integer :: n_par = 0
     logical :: azimuthal_dependence = .false.
     logical :: vamp_grids_defined = .false.
     logical :: sqrts_known = .false.
     logical :: sqrts_hat_known = .false.
     real(default) :: sqrts = 0
     real(default) :: sqrts_hat = 0
     real(default), dimension(:), allocatable :: x_strfun
     real(default), dimension(:), allocatable :: x_hi
     integer :: n_channels = 0
     integer :: n_bins = 0
     integer :: channel = 0
     logical :: lab_is_cm_frame = .true.
     type(lorentz_transformation_t) :: lt_cm_to_lab = identity
     logical :: old_phs_version = .false.
     type(process_status_t) :: status
     real(default), dimension(:,:), allocatable :: x
     real(default), dimension(:), allocatable :: phs_factor
     real(default), dimension(:), allocatable :: mass_in
     real(default) :: flux_factor = 0
     real(default) :: averaging_factor = 0
     real(default) :: sf_mapping_factor = 0
     real(default) :: phs_volume = 0
     real(default) :: vamp_phs_factor = 0
     real(default) :: sqme = 0
     real(default) :: reweighting_factor = 0
     real(default) :: sample_function_value = 0
     real(default) :: scale = 0
     real(default) :: fac_scale = 0
     real(default) :: ren_scale = 0  
     logical :: negative_weights = .false.
     type(qcd_parameters_t) :: qcd
     character(32) :: md5sum_alpha_s
     logical :: allow_s_channel_mapping = .false.
     type(strfun_chain_t) :: sfchain
     type(hard_interaction_t) :: hi
     type(evaluator_t) :: eval_trace
     type(evaluator_t) :: eval_beam_flows
     type(evaluator_t) :: eval_sqme
     type(evaluator_t) :: eval_flows
     logical :: fatal_beam_decay = .true.
     type(phs_forest_t) :: forest
     character(32) :: md5sum_phs = ""
     type(vamp_equivalences_t) :: vamp_eq
     integer, dimension(:), allocatable :: j_beam
     integer, dimension(:), allocatable :: j_in
     integer, dimension(:), allocatable :: j_out
     type(subevt_t) :: subevt
     type(var_list_t) :: var_list
     type(parse_node_t), pointer :: cut_pn => null ()
     type(parse_node_t), pointer :: weight_pn => null ()
     type(parse_node_t), pointer :: scale_pn => null ()
     type(parse_node_t), pointer :: fac_scale_pn => null ()
     type(parse_node_t), pointer :: ren_scale_pn => null ()
     type(eval_tree_t) :: cut_expr
     type(eval_tree_t) :: reweighting_expr
     type(eval_tree_t) :: scale_expr
     type(eval_tree_t) :: fac_scale_expr
     type(eval_tree_t) :: ren_scale_expr
     logical, dimension(:), allocatable :: active_channel
     type(vamp_grids) :: grids
     type(vamp_history), dimension(:), allocatable :: v_history
     type(vamp_history), dimension(:,:), allocatable :: v_histories
     type(integration_results_t) :: results
  end type process_t

  type :: process_p
     type(process_t), pointer :: ptr
  end type process_p

  type :: grid_parameters_t
     integer :: threshold_calls = 0
     integer :: min_calls_per_channel = 10
     integer :: min_calls_per_bin = 10
     integer :: min_bins = 3
     integer :: max_bins = 20
     logical :: stratified = .true.
     logical :: use_vamp_equivalences = .true.
     real(default) :: channel_weights_power = 0.25_default
  end type grid_parameters_t

  type :: md5sum_grids_t
     character(32) :: process    = ""
     character(32) :: model      = ""
     character(32) :: parameters = ""
     character(32) :: phs        = ""
     character(32) :: beams      = ""
     character(32) :: sf_list    = ""
     character(32) :: mappings   = ""
     character(32) :: cuts       = ""
     character(32) :: weight     = ""
     character(32) :: scale      = ""     
     character(32) :: fac_scale  = ""
     character(32) :: ren_scale  = ""     
     character(32) :: alpha_s    = ""     
  end type md5sum_grids_t

  type :: process_entry_t
     type(process_t) :: process
     type(process_entry_t), pointer :: next => null ()
  end type process_entry_t

  type :: process_store_t
     integer :: n = 0
     type(process_entry_t), pointer :: first => null ()
     type(process_entry_t), pointer :: last => null ()
     type(process_p), dimension(:), allocatable :: proc
  end type process_store_t


  type(process_store_t), save :: store


  interface process_set_strfun
     module procedure process_set_strfun_lhapdf  
     module procedure process_set_strfun_pdf_builtin
     module procedure process_set_strfun_isr
     module procedure process_set_strfun_epa
     module procedure process_set_strfun_ewa     
     module procedure process_set_strfun_circe1     
     module procedure process_set_strfun_circe2     
     module procedure process_set_strfun_escan
     module procedure process_set_strfun_beam_events
     module procedure process_set_strfun_user
  end interface

  interface operator(==)
     module procedure grid_parameters_eq
  end interface
  interface operator(/=)
     module procedure grid_parameters_ne
  end interface
interface
   double precision function alphasPDF (Q)
      double precision, intent(in) :: Q
   end function alphasPDF
end interface
  interface process_store_get_process_ptr
     module procedure process_store_get_process_ptr_int
     module procedure process_store_get_process_ptr_id
  end interface


contains

  subroutine integration_entry_init (entry, &
       process_type, pass, it, n_it, n_calls, improved, &
       integral, error, efficiency, chi2, grove_weight, &
       time_start, time_end)
    type(integration_entry_t), intent(out) :: entry
    integer, intent(in) :: process_type, pass, it, n_it, n_calls
    logical, intent(in) :: improved
    real(default), intent(in) :: integral, error, efficiency
    real(default), intent(in), optional :: chi2
    real(default), dimension(:), intent(in), optional :: grove_weight
    type(time_t), intent(in), optional :: time_start, time_end
    integer :: n_groves
    entry%process_type = process_type
    entry%pass = pass
    entry%it = it
    entry%n_it = n_it
    entry%n_calls = n_calls
    entry%improved = improved
    entry%integral = integral
    entry%error = error
    entry%efficiency = efficiency
    if (present (chi2)) &
         entry%chi2 = chi2
    if (present (grove_weight)) then
       n_groves = size (grove_weight)
       allocate (entry%grove_weight (n_groves))
       entry%grove_weight = grove_weight
    end if
    if (present (time_start) .and. present (time_end)) then
       entry%time_start = time_start
       entry%time_end = time_end
    end if
  end subroutine integration_entry_init

  elemental function integration_entry_get_pass (entry) result (n)
    integer :: n
    type(integration_entry_t), intent(in) :: entry
    n = entry%pass
  end function integration_entry_get_pass

  elemental function integration_entry_get_n_calls (entry) result (n)
    integer :: n
    type(integration_entry_t), intent(in) :: entry
    n = entry%n_calls
  end function integration_entry_get_n_calls

  elemental function integration_entry_get_integral (entry) result (int)
    real(default) :: int
    type(integration_entry_t), intent(in) :: entry
    int = entry%integral
  end function integration_entry_get_integral

  elemental function integration_entry_get_error (entry) result (err)
    real(default) :: err
    type(integration_entry_t), intent(in) :: entry
    err = entry%error
  end function integration_entry_get_error

  elemental function integration_entry_get_relative_error (entry) result (err)
    real(default) :: err
    type(integration_entry_t), intent(in) :: entry
    if (entry%integral /= 0) then
       err = entry%error / entry%integral
    else
       err = 0
    end if
  end function integration_entry_get_relative_error

  elemental function integration_entry_get_accuracy (entry) result (acc)
    real(default) :: acc
    type(integration_entry_t), intent(in) :: entry
    acc = accuracy (entry%integral, entry%error, entry%n_calls)
  end function integration_entry_get_accuracy

  elemental function accuracy (integral, error, n_calls) result (acc)
    real(default) :: acc
    real(default), intent(in) :: integral, error
    integer, intent(in) :: n_calls
    if (integral /= 0) then
       acc = error / integral * sqrt (real (n_calls, default))
    else
       acc = 0
    end if
  end function accuracy

  elemental function integration_entry_get_efficiency (entry) result (eff)
    real(default) :: eff
    type(integration_entry_t), intent(in) :: entry
    eff = entry%efficiency
  end function integration_entry_get_efficiency

  elemental function integration_entry_get_chi2 (entry) result (chi2)
    real(default) :: chi2
    type(integration_entry_t), intent(in) :: entry
    chi2 = entry%chi2
  end function integration_entry_get_chi2

  elemental function integration_entry_get_time_per_event (entry) result (tpe)
    real(default) :: tpe
    type(integration_entry_t), intent(in) :: entry
    real(default) :: time_in_seconds
    if (entry%n_calls /= 0 .and. entry%efficiency /= 0) then
       time_in_seconds = entry%time_end - entry%time_start
       tpe = time_in_seconds / entry%n_calls / entry%efficiency
    else
       tpe = 0
    end if
  end function integration_entry_get_time_per_event

  elemental function integration_entry_has_improved (entry) result (flag)
    logical :: flag
    type(integration_entry_t), intent(in) :: entry
    flag = entry%improved
  end function integration_entry_has_improved

  elemental function integration_entry_get_n_groves (entry) result (n_groves)
    integer :: n_groves
    type(integration_entry_t), intent(in) :: entry
    if (allocated (entry%grove_weight)) then
       n_groves = size (entry%grove_weight)
    else
       n_groves = 0
    end if
  end function integration_entry_get_n_groves

  subroutine write_header (process_type, unit, logfile)
    integer, intent(in) :: process_type
    integer, intent(in), optional :: unit
    logical, intent(in), optional :: logfile
    character(5) :: phys_unit
    integer :: u
    u = output_unit (unit);  if (u < 0)  return
    select case (process_type)
    case (PRC_DECAY);      phys_unit = "[GeV]"
    case (PRC_SCATTERING); phys_unit = "[fb] "
    case default
       phys_unit = ""
    end select
    write (msg_buffer, "(A)") &
         "It      Calls  Integral" // phys_unit // &
         " Error" // phys_unit // &
         "  Err[%]    Acc  Eff[%]   Chi2 N[It] |"
    call msg_message (unit=u, logfile=logfile)
  end subroutine write_header
       
  subroutine write_hline (unit)
    integer, intent(in), optional :: unit
    integer :: u
    u = output_unit (unit);  if (u < 0)  return
    write (u, "(A)")  "|" // (repeat ("-", 77)) // "|"
    flush (u)
  end subroutine write_hline
  
  subroutine write_dline (unit)
    integer, intent(in), optional :: unit
    integer :: u
    u = output_unit (unit);  if (u < 0)  return
    write (u, "(A)")  "|" // (repeat ("=", 77)) // "|"
    flush (u)
  end subroutine write_dline
  
  subroutine integration_entry_write (entry, unit, verbose)
    type(integration_entry_t), intent(in) :: entry
    integer, intent(in), optional :: unit
    logical, intent(in), optional :: verbose
    integer :: u
    character(1) :: star
    logical :: verb
    u = output_unit (unit);  if (u < 0)  return
    verb = .false.;  if (present (verbose))  verb = verbose
    if (.not. verb)  then
       if (entry%improved) then
          star = "*"
       else
          star = " "
       end if
1      format (1x, I3, 1x, I10, 1x, 1PE14.7, 1x, 1PE9.2, 1x, 2PF7.2, &
            1x, 0PF7.2, A1, 1x, 2PF6.2, 1x, 0PF7.2, 1x, I3)
       if (entry%n_it /= 1) then
          write (u, 1) &
               entry%it, &
               entry%n_calls, &
               entry%integral, &
               abs(entry%error), &
               abs(integration_entry_get_relative_error (entry)), &
               abs(integration_entry_get_accuracy (entry)), &
               star, &
               entry%efficiency, &
               entry%chi2, &
               entry%n_it
       else
          write (u, 1) &
               entry%it, &
               entry%n_calls, &
               entry%integral, &
               abs(entry%error), &
               abs(integration_entry_get_relative_error (entry)), &
               abs(integration_entry_get_accuracy (entry)), &
               star, &
               entry%efficiency
       end if
    else
       write (u, *)  "process_type = ", entry%process_type
       write (u, *)  "        pass = ", entry%pass
       write (u, *)  "          it = ", entry%it
       write (u, *)  "        n_it = ", entry%n_it
       write (u, *)  "     n_calls = ", entry%n_calls
       write (u, *)  "    improved = ", entry%improved
       write (u, *)  "    integral = ", entry%integral
       write (u, *)  "       error = ", entry%error
       write (u, *)  "  efficiency = ", entry%efficiency
       write (u, *)  "        chi2 = ", entry%chi2
       if (allocated (entry%grove_weight)) then
          write (u, *)  "    n_groves = ", size (entry%grove_weight)
          write (u, *)  "grove_weight = ", entry%grove_weight
       else
          write (u, *)  "    n_groves = 0"
       end if
    end if
    flush (u)
  end subroutine integration_entry_write

  subroutine integration_entry_read (entry, unit)
    type(integration_entry_t), intent(out) :: entry
    integer, intent(in) :: unit
    character(30) :: dummy
    character :: equals
    integer :: n_groves
    read (unit, *)  dummy, equals, entry%process_type
    read (unit, *)  dummy, equals, entry%pass
    read (unit, *)  dummy, equals, entry%it
    read (unit, *)  dummy, equals, entry%n_it
    read (unit, *)  dummy, equals, entry%n_calls
    read (unit, *)  dummy, equals, entry%improved
    read (unit, *)  dummy, equals, entry%integral
    read (unit, *)  dummy, equals, entry%error
    read (unit, *)  dummy, equals, entry%efficiency
    read (unit, *)  dummy, equals, entry%chi2
    read (unit, *)  dummy, equals, n_groves
    if (n_groves /= 0) then
       allocate (entry%grove_weight (n_groves))
       read (unit, *)  dummy, equals, entry%grove_weight
    end if
  end subroutine integration_entry_read
    
  subroutine integration_entry_write_grove_weights (entry, unit)
    type(integration_entry_t), intent(in) :: entry
    integer, intent(in), optional :: unit
    integer :: n_groves
    character(20) :: fmt
    integer :: u
    u = output_unit (unit);  if (u < 0)  return
    if (allocated (entry%grove_weight)) then
       n_groves = size (entry%grove_weight)
       write (fmt, "(A,I0,A)")  "(", n_groves, "(1x,I3))"
       write (u, fmt)  nint (entry%grove_weight * 100)
    end if
!   contains
!     function get_ifmt (n) result (fmt)
!       character(2) :: fmt
!       integer, intent(in) :: n
!       character(20) :: tmp_str
!       integer :: ilen
!       write (tmp_str, "(I0)")  n
!       ilen = len_trim (tmp_str) 
!       write (fmt, "(A1,I1)")  "I", ilen
!     end function get_ifmt
  end subroutine integration_entry_write_grove_weights

  function compute_average (entry, pass) result (result)
    type(integration_entry_t) :: result
    type(integration_entry_t), dimension(:), intent(in) :: entry
    integer, intent(in) :: pass
    integer :: i
    logical, dimension(size(entry)) :: mask
    real(default), dimension(size(entry)) :: ivar
    real(default) :: sum_ivar, variance
    result%process_type = entry(1)%process_type
    mask = entry%pass == pass
    result%it = maxval (entry%it, mask)
    result%n_it = count (mask)
    result%n_calls = sum (entry%n_calls, mask)
    where (entry%error /= 0)
       ivar = 1 / entry%error ** 2
    elsewhere
       ivar = 0
    end where
    sum_ivar = sum (ivar, mask)
    if (sum_ivar /= 0) then
       variance = 1 / sum_ivar
    else
       variance = 0
    end if
    result%integral = sum (entry%integral * ivar, mask) * variance
    result%error = sqrt (variance)
    if (result%n_it > 1) then
       result%chi2 = sum ((entry%integral - result%integral)**2 * ivar, mask) &
                     / (result%n_it - 1)
    end if
    do i = size (entry), 1, -1
       if (mask(i)) then
          result%efficiency = entry(i)%efficiency
          exit
       end if
    end do
  end function compute_average

  subroutine integration_results_init (results)
    type(integration_results_t), intent(out) :: results
    results%n_pass = 0
    results%n_it = 0
    allocate (results%entry (RESULTS_CHUNK_SIZE))
    allocate (results%average (RESULTS_CHUNK_SIZE))
  end subroutine integration_results_init

  subroutine integration_results_write (results, unit, verbose)
    type(integration_results_t), intent(in) :: results
    integer, intent(in), optional :: unit
    logical, intent(in), optional :: verbose
    logical :: verb
    integer :: u, n
    real(default) :: time_per_event
    u = output_unit (unit);  if (u < 0)  return
    verb = .false.;  if (present (verbose))  verb = verbose
    if (.not. verb) then
       call write_dline (unit)
       if (results%n_it /= 0) then
          call write_header (results%entry(1)%pass, unit)
          call write_dline (unit)
          do n = 1, results%n_it
             if (n > 1) then
                if (results%entry(n)%pass /= results%entry(n-1)%pass) then
                   call write_hline (unit)
                   call integration_entry_write &
                        (results%average(results%entry(n-1)%pass), unit)
                   call write_hline (unit)
                end if
             end if
             call integration_entry_write (results%entry(n), unit)
          end do
          call write_dline(unit)
          call integration_entry_write (results%average(results%n_pass), unit)
          call write_dline(unit)
       else
          call msg_message ("[WHIZARD integration results: empty]", unit)
       end if
       call write_dline (unit)
    else
       write (u, *)  "begin(integration_results)"
       write (u, *)  "  n_pass = ", results%n_pass
       write (u, *)  "    n_it = ", results%n_it
       if (results%n_it > 0) then
          write (u, *)  "begin(integration_pass)"
          do n = 1, results%n_it
             if (n > 1) then
                if (results%entry(n)%pass /= results%entry(n-1)%pass) then
                   write (u, *)  "end(integration_pass)"
                   write (u, *)  "begin(integration_pass)"
                end if
             end if
             write (u, *)  "begin(iteration)"
             call integration_entry_write (results%entry(n), unit, verb)
             write (u, *)  "end(iteration)"
          end do
          write (u, *)  "end(integration_pass)"
       end if
       write (u, *)  "end(integration_results)"
    end if
    flush (u)
  end subroutine integration_results_write

  subroutine integration_results_write_entry (results, it, unit)
    type(integration_results_t), intent(in) :: results
    integer, intent(in) :: it
    integer, intent(in), optional :: unit
    integer :: u
    u = output_unit (unit);  if (u < 0)  return
    if (it /= 0)  call integration_entry_write (results%entry(it), unit)
  end subroutine integration_results_write_entry

  subroutine integration_results_write_current (results, unit)
    type(integration_results_t), intent(in) :: results
    integer, intent(in), optional :: unit
    integer :: u, n
    u = output_unit (unit);  if (u < 0)  return
    n = results%n_it
    if (n /= 0)  call integration_entry_write (results%entry(n), unit)
  end subroutine integration_results_write_current

  subroutine integration_results_write_average (results, pass, unit)
    type(integration_results_t), intent(in) :: results
    integer, intent(in) :: pass
    integer, intent(in), optional :: unit
    integer :: u
    u = output_unit (unit);  if (u < 0)  return
    if (pass /= 0)  call integration_entry_write (results%average(pass), unit)
  end subroutine integration_results_write_average

  subroutine integration_results_write_current_average (results, unit)
    type(integration_results_t), intent(in) :: results
    integer, intent(in), optional :: unit
    integer :: u, n
    u = output_unit (unit);  if (u < 0)  return
    n = results%n_pass
    if (n /= 0)  call integration_entry_write (results%average(n), unit)
  end subroutine integration_results_write_current_average

  subroutine integration_results_write_grove_weights (results, unit)
    type(integration_results_t), intent(in) :: results
    integer, intent(in), optional :: unit
    integer :: u, i, n
    u = output_unit (unit);  if (u < 0)  return
    if (results%n_it /= 0) then
       call msg_message ("Phase-space grove weight history: " &
            // "(numbers in %)", unit)
       write (u, "(A9)", advance="no")  "| grove |"
          do i = 1, integration_entry_get_n_groves (results%entry(1))
          write (u, "(1x,I3)", advance="no")  i
       end do
       write (u, *)
       call write_dline (unit)
       do n = 1, results%n_it
          if (n > 1) then
             if (results%entry(n)%pass /= results%entry(n-1)%pass) then
                call write_hline (unit)
             end if
          end if
          write (u, "(1x,I6,1x,A1)", advance="no")  n, "|"
          call integration_entry_write_grove_weights (results%entry(n), unit)
       end do
    else
       call msg_message ("Channel weight history: [undefined]", unit)
    end if
    flush (u)
    call write_dline(unit)
  end subroutine integration_results_write_grove_weights

  subroutine integration_results_read (results, unit)
    type(integration_results_t), intent(out) :: results
    integer, intent(in) :: unit
    character(80) :: buffer
    character :: equals
    integer :: pass, it
    read (unit, *)  buffer
    if (trim (adjustl (buffer)) /= "begin(integration_results)") then
       call read_err ();  return
    end if
    read (unit, *)  buffer, equals, results%n_pass
    read (unit, *)  buffer, equals, results%n_it
    allocate (results%entry (results%n_it + RESULTS_CHUNK_SIZE))
    allocate (results%average (results%n_it + RESULTS_CHUNK_SIZE))
    it = 0
    do pass = 1, results%n_pass
       read (unit, *)  buffer
       if (trim (adjustl (buffer)) /= "begin(integration_pass)") then
          call read_err ();  return
       end if
       READ_ENTRIES: do
          read (unit, *)  buffer
          if (trim (adjustl (buffer)) /= "begin(iteration)") then
             exit READ_ENTRIES
          end if
          it = it + 1
          call integration_entry_read (results%entry(it), unit)
          read (unit, *)  buffer
          if (trim (adjustl (buffer)) /= "end(iteration)") then
             call read_err (); return
          end if
       end do READ_ENTRIES
       if (trim (adjustl (buffer)) /= "end(integration_pass)") then
          call read_err (); return
       end if
       results%average(pass) = compute_average (results%entry, pass)
    end do
    read (unit, *)  buffer
    if (trim (adjustl (buffer)) /= "end(integration_results)") then
       call read_err (); return
    end if
  contains
    subroutine read_err ()
      call msg_fatal ("Reading integration results from file: syntax error")
    end subroutine read_err
  end subroutine integration_results_read

  function integration_results_iterations_are_consistent &
       (results, pass, n_calls) result (flag)
    logical :: flag
    type(integration_results_t), intent(in) :: results
    integer, dimension(:), intent(in) :: pass, n_calls
    flag = all (results%entry(:results%n_it)%pass == pass(:results%n_it)) &
         .and. all (results%entry(:results%n_it)%n_calls &
                    == n_calls(:results%n_it))
  end function integration_results_iterations_are_consistent

  subroutine integration_results_expand (results)
    type(integration_results_t), intent(inout) :: results
    type(integration_entry_t), dimension(:), allocatable :: entry_tmp
    if (results%n_it == size (results%entry)) then
       allocate (entry_tmp (results%n_it))
       entry_tmp = results%entry
       deallocate (results%entry)
       allocate (results%entry (results%n_it + RESULTS_CHUNK_SIZE))
       results%entry(:results%n_it) = entry_tmp
       deallocate (entry_tmp)
    end if
    if (results%n_pass == size (results%average)) then
       allocate (entry_tmp (results%n_pass))
       entry_tmp = results%average
       deallocate (results%average)
       allocate (results%average (results%n_it + RESULTS_CHUNK_SIZE))
       results%average(:results%n_pass) = entry_tmp
       deallocate (entry_tmp)
    end if
  end subroutine integration_results_expand

  subroutine integration_results_append_entry (results, entry)
    type(integration_results_t), intent(inout) :: results
    type(integration_entry_t), intent(in) :: entry
    if (results%n_it == 0) then
       call integration_results_init (results)
       results%n_it = 1
       results%n_pass = 1
    else
       call integration_results_expand (results)
       if (entry%pass /= results%entry(results%n_it)%pass) &
            results%n_pass = results%n_pass + 1
       results%n_it = results%n_it + 1
    end if
    results%entry(results%n_it) = entry
    results%average(results%n_pass) = &
         compute_average (results%entry, entry%pass)
  end subroutine integration_results_append_entry

  subroutine integration_results_append (results, &
       process_type, pass, n_it, n_calls, &
       integral, error, efficiency, grove_weight, time_start, time_end)
    type(integration_results_t), intent(inout) :: results
    integer, intent(in) :: process_type, pass, n_it, n_calls
    real(default), intent(in) :: integral, error, efficiency
    real(default), dimension(:), intent(in), optional :: grove_weight
    type(time_t), intent(in), optional :: time_start, time_end
    logical :: improved
    type(integration_entry_t) :: entry
    if (results%n_it /= 0) then
       improved = abs(accuracy (integral, error, n_calls)) &
            < abs(integration_entry_get_accuracy (results%entry(results%n_it)))
    else
       improved = .true.
    end if
    call integration_entry_init (entry, &
         process_type, pass, results%n_it+1, n_it, n_calls, improved, & 
         integral, error, efficiency, grove_weight=grove_weight, &
         time_start=time_start, time_end=time_end)
    call integration_results_append_entry (results, entry)
  end subroutine integration_results_append
         
  function integration_results_exist (results) result (flag)
    logical :: flag
    type(integration_results_t), intent(in) :: results
    flag = results%n_pass > 0
  end function integration_results_exist

  function integration_results_get_n_calls (results) result (n_calls)
    integer :: n_calls
    type(integration_results_t), intent(in) :: results
    if (results%n_pass > 0) then
       n_calls = &
            integration_entry_get_n_calls (results%average(results%n_pass))
    else
       n_calls = 0
    end if
  end function integration_results_get_n_calls

  function integration_results_get_integral (results) result (integral)
    real(default) :: integral
    type(integration_results_t), intent(in) :: results
    if (results%n_pass > 0) then
       integral = &
            integration_entry_get_integral (results%average(results%n_pass))
    else
       integral = 0
    end if
  end function integration_results_get_integral

  function integration_results_get_error (results) result (error)
    real(default) :: error
    type(integration_results_t), intent(in) :: results
    if (results%n_pass > 0) then
       error = &
            integration_entry_get_error (results%average(results%n_pass))
    else
       error = 0
    end if
  end function integration_results_get_error

  function integration_results_get_accuracy (results) result (accuracy)
    real(default) :: accuracy
    type(integration_results_t), intent(in) :: results
    if (results%n_pass > 0) then
       accuracy = &
            integration_entry_get_accuracy (results%average(results%n_pass))
    else
       accuracy = 0
    end if
  end function integration_results_get_accuracy

  function integration_results_get_chi2 (results) result (chi2)
    real(default) :: chi2
    type(integration_results_t), intent(in) :: results
    if (results%n_pass > 0) then
       chi2 = &
            integration_entry_get_chi2 (results%average(results%n_pass))
    else
       chi2 = 0
    end if
  end function integration_results_get_chi2

  function integration_results_get_efficiency (results) result (efficiency)
    real(default) :: efficiency
    type(integration_results_t), intent(in) :: results
    if (results%n_pass > 0) then
       efficiency = &
            integration_entry_get_efficiency (results%average(results%n_pass))
    else
       efficiency = 0
    end if
  end function integration_results_get_efficiency

  function integration_results_get_time_per_event (results) result (s)
    real(default) :: s
    type(integration_results_t), intent(in) :: results
    if (results%n_pass /= 0) then
       s = integration_entry_get_time_per_event (results%entry(results%n_it))
    else
       s = 0
    end if
  end function integration_results_get_time_per_event

  function integration_results_get_current_pass (results) result (pass)
    integer :: pass
    type(integration_results_t), intent(in) :: results
    pass = results%n_pass
  end function integration_results_get_current_pass

  function integration_results_get_current_it (results) result (it)
    integer :: it
    type(integration_results_t), intent(in) :: results
    if (allocated (results%entry)) then
       it = count (results%entry%pass == results%n_pass)
    else
       it = 0
    end if
  end function integration_results_get_current_it

  function integration_results_get_md5sum (results) result (md5sum_results)
    character(32) :: md5sum_results
    type(integration_results_t), intent(in) :: results
    integer :: u
    u = free_unit ()
    open (unit = u, status = "scratch", action = "readwrite")
    call integration_results_write (results, u, verbose=.true.)
    rewind (u)
    md5sum_results = md5sum (u)
    close (u)
  end function integration_results_get_md5sum

  subroutine integration_results_write_driver (results, filename)
    type(integration_results_t), intent(in) :: results
    type(string_t), intent(in) :: filename
    type(string_t) :: file_basename, file_tex
    integer :: unit
    integer :: n, i, n_pass, pass
    integer, dimension(:), allocatable :: ipass
    real(default) :: ymin, ymax, yavg, ydif, y0, y1
    real(default) :: int, err
    file_basename = filename // ".history"
    file_tex = file_basename // ".tex"
    unit = free_unit ()
    open (unit=unit, file=char(file_tex), action="write", status="replace")
    n = results%n_it
    n_pass = results%n_pass
    allocate (ipass (results%n_pass))
    ipass(1) = 0
    pass = 2
    do i = 1, n-1
       if (integration_entry_get_pass (results%entry(i)) &
           /= integration_entry_get_pass (results%entry(i+1))) then
          ipass(pass) = i
          pass = pass + 1
       end if
    end do
    ymin = minval (integration_entry_get_integral (results%entry(:n)) &
                   - integration_entry_get_error (results%entry(:n)))
    ymax = maxval (integration_entry_get_integral (results%entry(:n)) &
                   + integration_entry_get_error (results%entry(:n)))
    yavg = (ymax + ymin) / 2
    ydif = (ymax - ymin)
    if (ydif * 1.5 > GML_MIN_RANGE_RATIO * yavg) then
       y0 = yavg - ydif * 0.75
       y1 = yavg + ydif * 0.75
    else
       y0 = yavg * (1 - GML_MIN_RANGE_RATIO / 2)
       y1 = yavg * (1 + GML_MIN_RANGE_RATIO / 2)
    end if
    write (unit, "(A)") "\documentclass{article}"
    write (unit, "(A)") "\usepackage{a4wide}"
    write (unit, "(A)") "\usepackage{gamelan}"
    write (unit, "(A)") "\usepackage{amsmath}"
    write (unit, "(A)") ""
    write (unit, "(A)") "\begin{document}"
    write (unit, "(A)") "\begin{gmlfile}"
    write (unit, "(A)") "\section*{Integration Results Display}"
    write (unit, "(A)") ""
    write (unit, "(A)") "Process: \verb|" // char (filename) // "|"
    write (unit, "(A)") ""
    write (unit, "(A)") "\vspace*{2\baselineskip}"
    write (unit, "(A)") "\unitlength 1mm"
    write (unit, "(A)") "\begin{gmlcode}"
    write (unit, "(A)") "  picture sym;  sym = fshape (circle scaled 1mm)();"
    write (unit, "(A)") "  color col.band;  col.band = 0.9white;"
    write (unit, "(A)") "  color col.eband;  col.eband = 0.98white;"
    write (unit, "(A)") "\end{gmlcode}"
    write (unit, "(A)") "\begin{gmlgraph*}(130,180)[history]"
    write (unit, "(A)") "  setup (linear, linear);"
    write (unit, "(A,I0,A)") "  history.n_pass = ", n_pass, ";"
    write (unit, "(A,I0,A)") "  history.n_it   = ", n, ";"
    write (unit, "(A,A,A)")  "  history.y0 = #""", char (mp_format (y0)), """;"
    write (unit, "(A,A,A)")  "  history.y1 = #""", char (mp_format (y1)), """;"
    write (unit, "(A)") &
         "  graphrange (#0.5, history.y0), (#(n+0.5), history.y1);"
    do pass = 1, n_pass
       write (unit, "(A,I0,A,I0,A)") &
            "  history.pass[", pass, "] = ", ipass(pass), ";"
       write (unit, "(A,I0,A,A,A)") &
            "  history.avg[", pass, "] = #""", &
            char (mp_format &
               (integration_entry_get_integral (results%average(pass)))), &
            """;"
       write (unit, "(A,I0,A,A,A)") &
            "  history.err[", pass, "] = #""", &
            char (mp_format &
               (integration_entry_get_error (results%average(pass)))), &
            """;"
       write (unit, "(A,I0,A,A,A)") &
            "  history.chi[", pass, "] = #""", &
            char (mp_format &
               (integration_entry_get_chi2 (results%average(pass)))), &
            """;"
    end do
    write (unit, "(A,I0,A,I0,A)") &
         "  history.pass[", n_pass + 1, "] = ", n, ";"
    write (unit, "(A)")  "  for i = 1 upto history.n_pass:"
    write (unit, "(A)")  "    if history.chi[i] greater one:"
    write (unit, "(A)")  "    fill plot ("
    write (unit, "(A)")  &
         "      (#(history.pass[i]  +.5), " &
         // "history.avg[i] minus history.err[i] times history.chi[i]),"
    write (unit, "(A)")  &
         "      (#(history.pass[i+1]+.5), " &
         // "history.avg[i] minus history.err[i] times history.chi[i]),"
    write (unit, "(A)")  &
         "      (#(history.pass[i+1]+.5), " &
         // "history.avg[i] plus history.err[i] times history.chi[i]),"
    write (unit, "(A)")  &
         "      (#(history.pass[i]  +.5), " &
         // "history.avg[i] plus history.err[i] times history.chi[i])"
    write (unit, "(A)")  "    ) withcolor col.eband fi;"
    write (unit, "(A)")  "    fill plot ("
    write (unit, "(A)")  &
         "      (#(history.pass[i]  +.5), history.avg[i] minus history.err[i]),"
    write (unit, "(A)")  &
         "      (#(history.pass[i+1]+.5), history.avg[i] minus history.err[i]),"
    write (unit, "(A)")  &
         "      (#(history.pass[i+1]+.5), history.avg[i] plus history.err[i]),"
    write (unit, "(A)")  &
         "      (#(history.pass[i]  +.5), history.avg[i] plus history.err[i])"
    write (unit, "(A)")  "    ) withcolor col.band;"
    write (unit, "(A)")  "    draw plot ("
    write (unit, "(A)")  &
         "      (#(history.pass[i]  +.5), history.avg[i]),"
    write (unit, "(A)")  &
         "      (#(history.pass[i+1]+.5), history.avg[i])"
    write (unit, "(A)")  "      ) dashed evenly;"
    write (unit, "(A)")  "  endfor"
    write (unit, "(A)")  "  for i = 1 upto history.n_pass + 1:"
    write (unit, "(A)")  "    draw plot ("
    write (unit, "(A)")  &
         "      (#(history.pass[i]+.5), history.y0),"
    write (unit, "(A)")  &
         "      (#(history.pass[i]+.5), history.y1)"
    write (unit, "(A)")  "      ) dashed withdots;"
    write (unit, "(A)")  "  endfor"
    do i = 1, n
       write (unit, "(A,I0,A,A,A,A,A)") "  plot (history) (#", &
          i, ", #""", &
          char (mp_format (integration_entry_get_integral (results%entry(i)))),&
          """) vbar #""", &
          char (mp_format (integration_entry_get_error (results%entry(i)))), &
          """;"
    end do
    write (unit, "(A)") "  draw piecewise from (history) " &
      // "withsymbol sym;"
    write (unit, "(A)") "  fullgrid.lr (5,20);"
    write (unit, "(A)") "  standardgrid.bt (n);"
    write (unit, "(A)") "\end{gmlgraph*}"
    write (unit, "(A)") "\end{gmlfile}"
    write (unit, "(A)") "\clearpage"
    write (unit, "(A)") "\begin{verbatim}"
    call integration_results_write (results, unit)
    write (unit, "(A)") "\end{verbatim}"
    write (unit, "(A)") "\end{document}"
    close (unit)
  end subroutine integration_results_write_driver

  subroutine integration_results_compile_driver (results, filename, os_data)
    type(integration_results_t), intent(in) :: results
    type(string_t), intent(in) :: filename
    type(os_data_t), intent(in) :: os_data
    integer :: unit, unit_dev, status
    type(string_t) :: file_basename
    type(string_t) :: file_tex, file_dvi, file_ps, file_pdf, file_mp
    type(string_t) :: setenv_tex, setenv_mp, pipe, pipe_dvi
    type(string_t) :: latex_opt, mpost_opt
    if (.not. os_data%event_analysis) then
       call msg_warning ("Skipping integration history display " &
           // "because latex or mpost is not available")
       return
    end if
    file_basename = filename // ".history"
    file_tex = file_basename // ".tex"
    file_dvi = file_basename // ".dvi"
    file_ps = file_basename // ".ps"
    file_pdf = file_basename // ".pdf"
    file_mp = file_basename // ".mp"
    call msg_message ("Creating integration history display "& 
         // char (file_ps) // " and " // char (file_pdf))
    BLOCK: do
       unit_dev = free_unit ()
       open (file = "/dev/null", unit = unit_dev, &
              action = "write", iostat = status)
       if (status /= 0) then
          pipe = ""
          pipe_dvi = ""
       else
          pipe = " > /dev/null"
          pipe_dvi = " 2>/dev/null 1>/dev/null"
       end if
       close (unit_dev)
       if (os_data%whizard_texpath /= "") then
          setenv_tex = &
               "TEXINPUTS=" // os_data%whizard_texpath // ":$TEXINPUTS "
          setenv_mp = &
               "MPINPUTS=" // os_data%whizard_texpath // ":$MPINPUTS "
       else
          setenv_tex = ""
          setenv_mp = ""
       end if
       call os_system_call (setenv_tex // os_data%latex // " " // &
            file_tex // pipe, status)
       if (status /= 0)  exit BLOCK
       if (os_data%gml /= "") then
          call os_system_call (setenv_mp // os_data%gml // " " // &
               file_mp // pipe, status)
       else 
          call msg_error ("Could not use GAMELAN/MetaPOST.")
          exit BLOCK
       end if
       if (status /= 0)  exit BLOCK
       call os_system_call (setenv_tex // os_data%latex // " " // &
             file_tex // pipe, status)
       if (status /= 0)  exit BLOCK
       if (os_data%event_analysis_ps) then
          call os_system_call (os_data%dvips // " " // &
             file_dvi // pipe_dvi, status)
          if (status /= 0)  exit BLOCK
       else
          call msg_warning ("Skipping PostScript generation because dvips " &
               // "is not available")
          exit BLOCK
       end if
       if (os_data%event_analysis_pdf) then
          call os_system_call (os_data%ps2pdf // " " // &
                  file_ps, status)
          if (status /= 0)  exit BLOCK
       else
          call msg_warning ("Skipping PDF generation because ps2pdf " &
               // "is not available")
          exit BLOCK
       end if
       exit BLOCK
    end do BLOCK
    if (status /= 0) then
       call msg_error ("Unable to compile integration history display")
    end if
  end subroutine integration_results_compile_driver

  subroutine process_status_write (status, unit)
    type(process_status_t), intent(in) :: status
    integer, intent(in), optional :: unit
    integer :: u
    u = output_unit (unit);  if (u < 0)  return
1   format (1x,A,L1,3x,I9)
    write (u, *)  "Process evaluation status (count):"
    write (u, 1) "  called                = ", status%called, &
         status%n_called
    write (u, 1) "  passed strfun_chain   = ", status%passed_strfun_chain, &
         status%n_passed_strfun_chain
    write (u, 1) "  passed mass_threshold = ", status%passed_mass_threshold, &
         status%n_passed_mass_threshold
    write (u, 1) "  passed kinematics     = ", status%passed_kinematics, &
         status%n_passed_kinematics
    write (u, 1) "  passed cuts           = ", status%passed_cuts, &
         status%n_passed_cuts
    write (u, 1) "  passed evaluation     = ", status%passed_evaluation, &
         status%n_passed_evaluation
  end subroutine process_status_write

  subroutine process_status_write_counters (status, unit)
    type(process_status_t), intent(in) :: status
    integer, intent(in), optional :: unit
    integer :: u
    u = output_unit (unit);  if (u < 0)  return
1   format (2x,A,1x,I9)
    call msg_message ("Process evaluation counters:", unit=u)
    write (msg_buffer, 1) "called                = ", &
         status%n_called
    call msg_message (unit=u)
    write (msg_buffer, 1) "passed strfun_chain   = ", &
         status%n_passed_strfun_chain
    call msg_message (unit=u)
    write (msg_buffer, 1) "passed mass_threshold = ", &
         status%n_passed_mass_threshold
    call msg_message (unit=u)
    write (msg_buffer, 1) "passed kinematics     = ", &
         status%n_passed_kinematics
    call msg_message (unit=u)
    write (msg_buffer, 1) "passed cuts           = ", &
         status%n_passed_cuts
    call msg_message (unit=u)
    write (msg_buffer, 1) "passed evaluation     = ", &
         status%n_passed_evaluation 
    call msg_message (unit=u)
  end subroutine process_status_write_counters

  subroutine process_status_reset_flags (status)
    type(process_status_t), intent(inout) :: status
    status%called = .false.
    status%passed_strfun_chain = .false.
    status%passed_mass_threshold = .false.
    status%passed_kinematics = .false.
    status%passed_cuts = .false.
    status%passed_evaluation = .false.
  end subroutine process_status_reset_flags

  subroutine process_status_reset_counters (status)
    type(process_status_t), intent(out) :: status
  end subroutine process_status_reset_counters
    
  subroutine process_status_called (status)
    type(process_status_t), intent(inout) :: status
    status%called = .true.
    status%n_called = status%n_called + 1
  end subroutine process_status_called

  subroutine process_status_passed_strfun_chain (status)
    type(process_status_t), intent(inout) :: status
    status%passed_strfun_chain = .true.
    status%n_passed_strfun_chain = status%n_passed_strfun_chain + 1
  end subroutine process_status_passed_strfun_chain

  subroutine process_status_passed_mass_threshold (status)
    type(process_status_t), intent(inout) :: status
    status%passed_mass_threshold = .true.
    status%n_passed_mass_threshold = status%n_passed_mass_threshold + 1
  end subroutine process_status_passed_mass_threshold

  subroutine process_status_passed_kinematics (status)
    type(process_status_t), intent(inout) :: status
    status%passed_kinematics = .true.
    status%n_passed_kinematics = status%n_passed_kinematics + 1
  end subroutine process_status_passed_kinematics

  subroutine process_status_passed_cuts (status)
    type(process_status_t), intent(inout) :: status
    status%passed_cuts = .true.
    status%n_passed_cuts = status%n_passed_cuts + 1
  end subroutine process_status_passed_cuts

  subroutine process_status_passed_evaluation (status)
    type(process_status_t), intent(inout) :: status
    status%passed_evaluation = .true.
    status%n_passed_evaluation = status%n_passed_evaluation + 1
  end subroutine process_status_passed_evaluation

  subroutine qcd_parameters_setup (qcd, lhapdf_status, var_list)
    type(qcd_parameters_t), intent(inout) :: qcd
    type(lhapdf_status_t), intent(inout) :: lhapdf_status
    type(var_list_t), intent(in), target :: var_list
    type(string_t) :: lhapdf_file, lhapdf_dir
    qcd%alpha_s_is_fixed = &
         var_list_get_lval (var_list, var_str ("?alpha_s_is_fixed"))
    qcd%order = &
         var_list_get_ival (var_list, var_str ("alpha_s_order"))
    qcd%nf = &
         var_list_get_ival (var_list, var_str ("alpha_s_nf"))
    qcd%alpha_s_from_mz = &
         var_list_get_lval (var_list, var_str ("?alpha_s_from_mz"))
    qcd%alpha_s_from_lhapdf = &
         var_list_get_lval (var_list, var_str ("?alpha_s_from_lhapdf"))
    if (qcd%alpha_s_from_lhapdf) then
       if (LHAPDF_AVAILABLE) then
          qcd%lhapdf_set = 1
          lhapdf_dir = var_list_get_sval (var_list, &
               var_str ("$lhapdf_dir"))  ! $
          lhapdf_file = var_list_get_sval (var_list, &
               var_str ("$lhapdf_file"))  ! $
          qcd%lhapdf_member = var_list_get_ival (var_list, &
               var_str ("lhapdf_member"))
          call lhapdf_init (lhapdf_status, &
               qcd%lhapdf_set, lhapdf_dir, lhapdf_file, qcd%lhapdf_member)
       else             
          call msg_error &
               ("LHAPDF not linked: reset alpha_s_from_lhapdf to false")
          qcd%alpha_s_from_lhapdf = .false.
       end if
    end if
    qcd%mz_is_known = &
         var_list_is_known (var_list, var_str ("mZ"))
    if (qcd%mz_is_known)  qcd%mz = &
         var_list_get_rval (var_list, var_str ("mZ"))
    qcd%alpha_s_mz_is_known = &
         var_list_is_known (var_list, var_str ("alphas"))
    if (qcd%alpha_s_mz_is_known)  qcd%alpha_s_mz = &
         var_list_get_rval (var_list, var_str ("alphas"))
    qcd%lambda = &
         var_list_get_rval (var_list, var_str ("lambda_qcd"))
  end subroutine qcd_parameters_setup

  subroutine qcd_parameters_write (qcd, unit)
    type(qcd_parameters_t), intent(in) :: qcd
    integer, intent(in), optional :: unit
    integer :: u
    u = output_unit (unit)
    write (u, *)  "QCD coupling parameters ="
    write (u, *)  "  alpha-s is fixed = ", qcd%alpha_s_is_fixed
    if (.not. qcd%alpha_s_is_fixed) then
       if (qcd%alpha_s_from_lhapdf) then
          write (u, *)  "  alpha-s from LHAPDF"
          write (u, *)  "  PDF group        = ", qcd%lhapdf_set
          write (u, *)  "  PDF member       = ", qcd%lhapdf_member
       else
          write (u, *)  "  LLA order        = ", qcd%order
          write (u, *)  "  active flavors   = ", qcd%nf
          write (u, *)  "  use alpha-s (mZ) = ", qcd%alpha_s_from_mz
          if (qcd%alpha_s_from_mz) then
             write (u, *)  "  mZ is known      = ", qcd%mz_is_known
             if (qcd%mz_is_known) then
                write (u, *)  "  mZ               = ", qcd%mz
             end if
             write (u, *)  "  as(mZ) is known  = ", qcd%alpha_s_mz_is_known
             if (qcd%alpha_s_mz_is_known) then
                write (u, *)  "  alpha-s (mZ)     = ", qcd%alpha_s_mz
             end if
          else
             write (u, *)  "  Lambda_QCD       = ", qcd%lambda
          end if
       end if
       write (u, *)  "  alpha-s (scale)  = ", qcd%alpha_s_at_scale
    end if
  end subroutine qcd_parameters_write

  function qcd_parameters_get_md5sum (qcd) result (md5)
    character(32) :: md5
    type(qcd_parameters_t), intent(in) :: qcd
    integer :: u
    u = free_unit ()
    open (unit=u, status="scratch")
    call qcd_parameters_write (qcd, u)
    rewind (u)
    md5 = md5sum (u)
    close (u)
  end function qcd_parameters_get_md5sum

  subroutine qcd_parameters_update_alpha_s (qcd, scale)
    type(qcd_parameters_t), intent(inout) :: qcd
    real(default), intent(in) :: scale
    real(default) :: alpha_s
    if (.not. qcd%alpha_s_is_fixed) then
       if (qcd%alpha_s_from_lhapdf) then
          alpha_s = alphasPDF (dble (scale))
       else
          if (qcd%alpha_s_from_mz) then
             if (qcd%alpha_s_mz_is_known) then
                if (qcd%mz_is_known) then
                   alpha_s = running_as (scale, &
                        al_mz = qcd%alpha_s_mz, &
                        mz = qcd%mz, &
                        order = qcd%order, &
                        nf = real (qcd%nf, default))
                else
                   alpha_s = running_as (scale, &
                        al_mz = qcd%alpha_s_mz, &
                        order = qcd%order, &
                        nf = real (qcd%nf, default))
                end if
             else
                if (qcd%mz_is_known) then
                   alpha_s = running_as (scale, &
                        mz = qcd%mz, &
                        order = qcd%order, &
                        nf = real (qcd%nf, default))
                else
                   alpha_s = running_as (scale, &
                        order = qcd%order, &
                        nf = real (qcd%nf, default))
                end if                
             end if
          else
             alpha_s = running_as_lam (real (qcd%nf, default), scale, &
                  lambda_qcd = qcd%lambda, &
                  order = qcd%order)
          end if   
       end if
       qcd%alpha_s_at_scale = alpha_s
    end if
  end subroutine qcd_parameters_update_alpha_s

  subroutine process_init &
       (process, prc_lib, process_lib_index, process_store_index, &
        process_id, model, lhapdf_status, var_list, use_beams)
    type(process_t), intent(out), target :: process
    type(process_library_t), intent(in), target :: prc_lib
    integer, intent(in) :: process_lib_index
    integer, intent(in) :: process_store_index
    type(string_t), intent(in) :: process_id
    type(model_t), intent(in), target :: model
    type(lhapdf_status_t), intent(inout) :: lhapdf_status
    type(var_list_t), intent(in), target :: var_list
    logical, intent(in), optional :: use_beams
    integer :: n_in, n_out, n_tot
    integer :: n_beam
    process%prc_lib => prc_lib
    process%lib_index = process_lib_index
    process%store_index = process_store_index
    process%id = process_id
    process%md5sum = process_library_get_process_md5sum &
         (process%prc_lib, process%lib_index)
    call hard_interaction_init &
         (process%hi, prc_lib, process_lib_index, process_id, model)
    process%has_matrix_element = hard_interaction_get_n_flv (process%hi) /= 0
    process%use_hi_color_factors = &
         var_list_get_lval (var_list, var_str ("?read_color_factors"))
    process%model => hard_interaction_get_model_ptr (process%hi)
    if (.not. hard_interaction_is_valid (process%hi)) then
       return
    else
       process%id = hard_interaction_get_id (process%hi)
       if (.not. process%has_matrix_element) then
          process%initialized = .true.
          return
       end if
    end if
    if (present (use_beams)) then
       process%use_beams = use_beams
       process%has_extra_evaluators = use_beams
    end if
    n_in  = hard_interaction_get_n_in  (process%hi)
    n_out = hard_interaction_get_n_out (process%hi)
    n_tot = hard_interaction_get_n_tot (process%hi)
    select case (n_in)
    case (1);  process%type = PRC_DECAY
    case (2);  process%type = PRC_SCATTERING
    end select
    allocate (process%flv_in (n_in))
    call flavor_init (process%flv_in, &
         hard_interaction_get_first_pdg_in (process%hi), process%model)
    allocate (process%flv_out (n_out))
    call flavor_init (process%flv_out, &
         hard_interaction_get_first_pdg_out (process%hi), process%model)
    allocate (process%mass_in (n_in))
    process%mass_in = flavor_get_mass (process%flv_in)
    if (process%use_beams) then
       n_beam = n_in
       process%averaging_factor = 1
    else
       n_beam = 0
       process%averaging_factor = &
            1._default / product (flavor_get_multiplicity (process%flv_in))
    end if
    call process_assign_global_var_list (process, var_list)

!    process%old_phs_version = &
!         var_list_get_lval (var_list, var_str ("?old_phs_version"))

    process%negative_weights = &
         var_list_get_lval (var_list, var_str ("?negative_weights"))
    process%fatal_beam_decay = &
         var_list_get_lval (var_list, var_str ("?fatal_beam_decay"))

    call qcd_parameters_setup (process%qcd, lhapdf_status, var_list)
    process%md5sum_alpha_s = qcd_parameters_get_md5sum (process%qcd)

    call var_list_append_int (process%var_list, &
         var_str ("n_in"),  n_in, intrinsic=.true.)
    call var_list_append_int (process%var_list, &
         var_str ("n_out"), n_out, intrinsic=.true.)
    call var_list_append_int (process%var_list, &
         var_str ("n_tot"), n_tot, intrinsic=.true.)
    call var_list_append_real_ptr (process%var_list, &
         var_str ("sqrts"), process%sqrts, process%sqrts_known, &
         intrinsic=.true.)
    call var_list_append_real_ptr (process%var_list, &
         var_str ("sqrts_hat"), process%sqrts_hat, process%sqrts_hat_known, &
         intrinsic=.true.)
    allocate (process%j_beam (n_beam))
    allocate (process%j_in (n_in))
    allocate (process%j_out (n_out))
    call subevt_init (process%subevt, n_beam + n_in + n_out)
!    call integration_results_init (process%results)
    process%initialized = .true.
  end subroutine process_init

  subroutine process_assign_global_var_list (process, var_list)
    type(process_t), intent(inout) :: process
    type(var_list_t), intent(in), optional, target :: var_list
    type(var_list_t), pointer :: var_list_snapshot
    var_list_snapshot => var_list_get_next_ptr (process%var_list)
    if (associated (var_list_snapshot)) then
       call var_list_final (var_list_snapshot)
       deallocate (var_list_snapshot)
    end if
    allocate (var_list_snapshot)
    call var_list_link (process%var_list, var_list_snapshot)
    if (present (var_list)) then
       call var_list_init_snapshot (var_list_snapshot, var_list)
    else
       call var_list_init_snapshot (var_list_snapshot, &
            model_get_var_list_ptr (process%model))
    end if
  end subroutine process_assign_global_var_list

  recursive subroutine process_final (process)
    type(process_t), intent(inout), target :: process
    call process_delete_copies (process)
    process%initialized = .false.
    process%type = PRC_UNKNOWN
    process%sqrts_known = .false.
    process%sqrts_hat_known = .false.
    call strfun_chain_final (process%sfchain)
    call hard_interaction_final (process%hi)
    call evaluator_final (process%eval_trace)
    call evaluator_final (process%eval_beam_flows)
    call evaluator_final (process%eval_sqme)
    call evaluator_final (process%eval_flows)
    call phs_forest_final (process%forest)
    call vamp_equivalences_final (process%vamp_eq)
    if (process%is_original) then
       call var_list_final (process%var_list)
       call eval_tree_final (process%cut_expr)
       call eval_tree_final (process%reweighting_expr)
       call eval_tree_final (process%scale_expr)
       call eval_tree_final (process%fac_scale_expr)
       call eval_tree_final (process%ren_scale_expr)       
    end if
    if (process%vamp_grids_defined) then
       call vamp_delete_grids (process%grids)
    end if
    call process_final_vamp_history (process)
  end subroutine process_final

  subroutine process_write &
       (process, unit, verbose, show_momentum_sum, show_mass)
    type(process_t), intent(in) :: process
    integer, intent(in), optional :: unit
    logical, intent(in), optional :: verbose, show_momentum_sum, show_mass
    integer :: u, i
    u = output_unit (unit);  if (u < 0)  return
    write (u, "(A)")  repeat ("=", 72)
    write (u, *)  "Process data:", process%lib_index, &
         "(", char (process%id), ")"
    select case (process%type)
    case (PRC_UNKNOWN);     write (u, *) "  [unknown]"
    case (PRC_DECAY);       write (u, *) "  [decay]"
    case (PRC_SCATTERING);  write (u, *) "  [scattering]"
    end select
    write (u, *)  "  is cascade decay        = ", process%is_cascade_decay
    write (u, *)  "  use separate beam setup = ", process%use_beams
    call beam_data_write (process%beam_data, u)
    if (process%use_beams) then
       write (u, *)  "  number of structure functions  = ", process%n_strfun
       write (u, *)  "  number of strfun parameters    = ", process%n_par_strfun
    end if
    write (u, *)  "  number of process parameters   = ", process%n_par_hi
    write (u, *)  "  number of parameters total     = ", process%n_par
    write (u, *)  "  number of integration channels = ", process%n_channels
    write (u, *)  "  number of bins per channel     = ", process%n_bins
    if (process%sqrts_known) then
       write (u, *)  "  c.m. energy (sqrts)     = ", process%sqrts
    else
       write (u, *)  "  c.m. energy (sqrts)     = [unknown]"
    end if
    write (u, *)  repeat ("-", 72)
    call process_status_write (process%status, u)
    write (u, *)  repeat ("-", 72)
    write (u, *)  "Evaluation results:"
    if (process%sqrts_hat_known) then
       write (u, *)  "  c.m. energy (sqrts_hat) = ", process%sqrts_hat
    else
       write (u, *)  "  c.m. energy (sqrts_hat) = [unknown]"
    end if
    write (u, "(1x,A)", advance="no")  "  Colliding partons       = "
    if (allocated (process%flv_in)) then
       do i = 1, size (process%flv_in)
          if (i == 2)  write (u, "(1x)", advance="no")
          call flavor_write (process%flv_in(i), u)
       end do
       write (u, *)
    else
       write (u, *)  "[undefined]"
    end if
    if (allocated (process%mass_in)) then
       write (u, *)  "  Incoming parton masses  = ", process%mass_in
    else
       write (u, *)  "  Incoming parton masses  = [unknown]"
    end if
    write (u, *)  "  In-state flux factor    = ", process%flux_factor
    write (u, *)  "  Strfun mapping factor   = ", process%sf_mapping_factor
    if (.not. process%use_beams) then
       write (u, *)  "  Spin averaging factor   = ", process%averaging_factor
    end if
    write (u, *)  "  VAMP phs factor         = ", process%vamp_phs_factor
    write (u, *)  "  Phase-space volume      = ", process%phs_volume
    write (u, *)  "  Squared matrix element  = ", process%sqme
    write (u, *)  "  Reweighting factor      = ", process%reweighting_factor
    write (u, *)  "  Sample-function value   = ", &
         process%sample_function_value
    write (u, *)  repeat ("-", 72)
    if (process%use_beams) then
       write (u, *)  "Structure function parameters ="
       if (allocated (process%x_strfun)) then
          write (u, *)  process%x_strfun
       else
          write (u, *)  "[empty]"
       end if
    end if
    write (u, *)  "  General scale           = ", process%scale
    write (u, *)  "  Factorization scale     = ", process%fac_scale
    write (u, *)  "  Renormalization scale   = ", process%ren_scale    
    write (u, *)  repeat ("-", 72)
    call qcd_parameters_write (process%qcd, u)
    write (u, *)  repeat ("-", 72)
    write (u, *)  "Phase-space integration parameters (input) ="
    if (allocated (process%x_hi)) then
       write (u, *)  process%x_hi
    else
       write (u, *)  "[empty]"
    end if
    write (u, *)  "Integration channel =", process%channel
    write (u, *)  "Phase-space integration parameters (complete) ="
    if (allocated (process%x)) then
       do i = 1, size (process%x, 2)
          write (u, *)  process%x(:,i)
       end do
    else
       write (u, *)  "[empty]"
    end if
    if (.not. process%lab_is_cm_frame) then
       write (u, *)  "Tranformation c.m. -> lab ="
       call lorentz_transformation_write (process%lt_cm_to_lab, u)
    end if
    write (u, *)  "Channels: phase-space factors ="
    if (allocated (process%phs_factor)) then
       write (u, *)  process%phs_factor
    else
       write (u, *) "[not allocated]"
    end if
    write (u, "(A)")  repeat ("-", 72)
    if (process%use_beams) then
       call strfun_chain_write &
            (process%sfchain, unit, verbose, show_momentum_sum, show_mass)
       write (u, *)
       write (u, *) "Allow s-channel mapping = ", &
            process%allow_s_channel_mapping
       write (u, "(A)")  repeat ("-", 72)
       write (u, "(A)") "Incoming beams with all color contractions"
       call evaluator_write &
            (process%eval_beam_flows, unit, verbose, show_momentum_sum, show_mass)
       write (u, "(A)")  repeat ("-", 72)
    end if
    call hard_interaction_write &
         (process%hi, unit, verbose, show_momentum_sum, show_mass)
    write (u, "(A)")  repeat ("-", 72)
    if (process%has_extra_evaluators) then
       write (u, "(A)") "Trace including color factors (beams + strfun + hard interaction)"
       call evaluator_write &
            (process%eval_trace, unit, verbose, show_momentum_sum, show_mass)
       write (u, "(A)")  repeat ("-", 72)
       write (u, "(A)") "Exclusive sqme including color factors (beams + strfun + hard interaction)"
       call evaluator_write &
            (process%eval_sqme, unit, verbose, show_momentum_sum, show_mass)
       write (u, "(A)")  repeat ("-", 72)
       write (u, "(A)") "Color flow coefficients (beams + strfun + hard interaction)"
       call evaluator_write &
            (process%eval_flows, unit, verbose, show_momentum_sum, show_mass)
       write (u, "(A)")  repeat ("-", 72)
    end if
    call phs_forest_write (process%forest, unit)
    write (u, "(A)")  repeat ("-", 72)
    call vamp_equivalences_write (process%vamp_eq, unit)
    write (u, "(A)")  repeat ("-", 72)
    write (u, "(A)")  "Subevent used by cuts, weight, and scale:"
    write (u, "(A)", advance="no") &
         "  Beam indices (in the trace evaluator): "
    if (allocated (process%j_beam)) then
       write (u, *)  process%j_beam
    else
       write (u, *) "[undefined]"
    end if
    write (u, "(A)", advance="no") &
         "  In-parton indices (in the trace evaluator): "
    if (allocated (process%j_out)) then
       write (u, *)  process%j_in
    else
       write (u, *) "[undefined]"
    end if
    write (u, "(A)", advance="no") &
         "  Out-parton indices (in the trace evaluator): "
    if (allocated (process%j_out)) then
       write (u, *)  process%j_out
    else
       write (u, *) "[undefined]"
    end if
    call subevt_write (process%subevt, unit)
    write (u, "(A)")  repeat ("-", 72)
    call var_list_write (process%var_list, unit)
    write (u, "(A)")  repeat ("-", 72)
    write (u, "(A)")  "Cut expression:"
    call eval_tree_write (process%cut_expr, unit)
    write (u, "(A)")  repeat ("-", 72)
    write (u, "(A)")  "Weight expression:"
    call eval_tree_write (process%reweighting_expr, unit)
    write (u, "(A)")  repeat ("-", 72)
    write (u, "(A)")  "General scale expression:"
    call eval_tree_write (process%scale_expr, unit)
    write (u, "(A)")  repeat ("-", 72)
    write (u, "(A)")  "Factorization scale expression:"
    call eval_tree_write (process%fac_scale_expr, unit)
    write (u, "(A)")  repeat ("-", 72)
    write (u, "(A)")  "Renormalization scale expression:"    
    call eval_tree_write (process%ren_scale_expr, unit)
    write (u, "(A)")  repeat ("-", 72)    
    if (process%vamp_grids_defined) then
       call vamp_write_grids (process%grids, u)
    else
       write (u, "(A)")  "VAMP grids: [empty]"
    end if
    write (u, "(A)")  repeat ("-", 72)
    if (allocated (process%v_history)) then
       call msg_message (" Global history [vamp]:", unit=u)
       call vamp_write_history (u, process%v_history)
    else
       call msg_message (" Global history [vamp]: [undefined]", unit=u)
    end if
    write (u, "(A)")  repeat ("-", 72)
    if (allocated (process%v_histories)) then
       call msg_message (" Channel histories [vamp]:", unit=u)
       call vamp_write_history (u, process%v_histories)
    else
       call msg_message (" Channel histories [vamp]: [undefined]", unit=u)
    end if
    write (u, *)
    call integration_results_write (process%results, unit)
    call integration_results_write_grove_weights (process%results, unit)
  end subroutine process_write

  subroutine process_write_log (process, unit)
    type(process_t), intent(in) :: process
    integer, intent(in), optional :: unit
    integer :: u
    u = output_unit (unit)
    write (u, "(A)")  repeat ("#", 79)
    write (u, *)  "Process ID = '" // char (process%id) // "'"
    write (u, "(A)")  repeat ("#", 79)
    write (u, *)  "Integral   = ", process_get_integral (process)
    write (u, *)  "Error      = ", process_get_error (process)
    write (u, *)  "Accuracy   = ", process_get_accuracy (process)
    write (u, *)  "Chi2       = ", process_get_chi2 (process)
    write (u, *)  "Efficiency = ", process_get_efficiency (process)
    write (u, *)  "Time/evt   = ", process_get_time_per_event (process)
    call integration_results_write (process%results, unit)
    write (u, "(A)")  repeat ("#", 79)
    call process_status_write_counters (process%status, unit)
    write (u, "(A)")  repeat ("#", 79)
    call integration_results_write_grove_weights (process%results, unit)
    write (u, "(A)")  repeat ("#", 79)
    call beam_data_write (process%beam_data, u)
    write (u, "(A)")  repeat ("#", 79)
    write (u, "(A)")  "Cut expression:"
    call eval_tree_write (process%cut_expr, unit)
    write (u, "(A)")  repeat ("-", 79)
    write (u, "(A)")  "Weight expression:"
    call eval_tree_write (process%reweighting_expr, unit)
    write (u, "(A)")  repeat ("-", 79)
    write (u, "(A)")  "General scale expression:"
    call eval_tree_write (process%scale_expr, unit)    
    write (u, "(A)")  repeat ("-", 79)
    write (u, "(A)")  "Factorization scale expression:"
    call eval_tree_write (process%fac_scale_expr, unit)
    write (u, "(A)")  repeat ("-", 79)
    write (u, "(A)")  "Renormalization scale expression:"
    call eval_tree_write (process%ren_scale_expr, unit)    
    write (u, "(A)")  repeat ("#", 79)
    write (u, "(A)")  "Summary of quantum-number states:"
    write (u, "(A)")  " + sign: allowed and contributing"
    write (u, "(A)")  " no +  : switched off at runtime"
    write (u, "(A)")  repeat ('-', 79)
    call hard_interaction_write_state_summary (process%hi, unit)
    write (u, "(A)")  repeat ("#", 79)
    if (allocated (process%v_history)) then
       call msg_message ("Global history [vamp]:", unit=u)
       call vamp_write_history (u, process%v_history)
    else
       call msg_message ("Global history [vamp]: [undefined]", unit=u)
    end if
    write (u, "(A)")  repeat ("-", 72)
    if (allocated (process%v_histories)) then
       call msg_message ("Channel histories [vamp]:", unit=u)
       call vamp_write_history (u, process%v_histories)
    else
       call msg_message ("Channel histories [vamp]: [undefined]", unit=u)
    end if
    write (u, "(A)")  repeat ("-", 72)
    call msg_message ("Equivalences between channels", unit=u)
    call vamp_equivalences_write (process%vamp_eq, unit=u)
    write (u, "(A)")  repeat ("#", 79)
    write (u, "(A)")  "Variable list:"
    call var_list_write (process%var_list, unit)
    write (u, "(A)")  repeat ("#", 79)
  end subroutine process_write_log

  subroutine process_write_logfile (process)
    type(process_t), intent(in) :: process
    type(string_t) :: filename
    integer :: unit
    unit = free_unit ()
    filename = process%id // ".log"
    open (unit = unit, file = char (filename), action = "write", &
          status = "replace")
    call process_write_log (process, unit)
    close (unit)
  end subroutine process_write_logfile

  subroutine process_display_integration_history (process, os_data, vis_history)
    type(process_t), intent(in) :: process
    type(os_data_t), intent(in) :: os_data
    logical, intent(in) :: vis_history
    call integration_results_write_driver (process%results, process%id)
    if (vis_history) call integration_results_compile_driver &
          (process%results, process%id, os_data)
  end subroutine process_display_integration_history

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

  function process_is_valid (process) result (flag)
    logical :: flag
    type(process_t), intent(in) :: process
    flag = process%initialized
  end function process_is_valid

  function process_has_matrix_element (process) result (flag)
    logical :: flag
    type(process_t), intent(in) :: process
    flag = process%has_matrix_element
  end function process_has_matrix_element

  function process_has_integral (process) result (flag)
    logical :: flag
    type(process_t), intent(in) :: process
    flag = integration_results_exist (process%results)
  end function process_has_integral

  function process_uses_beams (process) result (flag)
    logical :: flag
    type(process_t), intent(in) :: process
    flag = process%use_beams
  end function process_uses_beams

  function process_get_id (process) result (process_id)
    type(string_t) :: process_id
    type(process_t), intent(in) :: process
    process_id = process%id
  end function process_get_id

  function process_get_lib_index (process) result (index)
    integer :: index
    type(process_t), intent(in) :: process
    index = process%lib_index
  end function process_get_lib_index

  function process_get_store_index (process) result (index)
    integer :: index
    type(process_t), intent(in) :: process
    index = process%store_index
  end function process_get_store_index

  function process_get_md5sum (process) result (md5sum)
    character(32) :: md5sum
    type(process_t), intent(in) :: process
    md5sum = process%md5sum
  end function process_get_md5sum

  function process_get_md5sum_parameters (process) result (md5sum)
    character(32) :: md5sum
    type(process_t), intent(in) :: process
    md5sum = model_get_parameters_md5sum (process%model)
  end function process_get_md5sum_parameters

  function process_get_md5sum_results (process) result (md5sum)
    character(32) :: md5sum
    type(process_t), intent(in) :: process
    md5sum = integration_results_get_md5sum (process%results)
  end function process_get_md5sum_results

  function process_get_md5sum_polarized (process) result (md5sum)
    character(32) :: md5sum
    type(process_t), intent(in) :: process
    md5sum = model_get_polarized_md5sum (process%model)
  end function process_get_md5sum_polarized

  function process_get_model_ptr (process) result (model)
    type(model_t), pointer :: model
    type(process_t), intent(in) :: process
    model => process%model
  end function process_get_model_ptr

  pure function process_get_n_in (process) result (n)
    integer :: n
    type(process_t), intent(in) :: process
    n = hard_interaction_get_n_in (process%hi)
  end function process_get_n_in

  pure function process_get_n_out (process) result (n)
    integer :: n
    type(process_t), intent(in) :: process
    n = hard_interaction_get_n_out (process%hi)
  end function process_get_n_out

  pure function process_get_n_tot (process) result (n)
    integer :: n
    type(process_t), intent(in) :: process
    n = hard_interaction_get_n_tot (process%hi)
  end function process_get_n_tot
    
  pure function process_get_n_flv (process) result (n)
    integer :: n
    type(process_t), intent(in) :: process
    n = hard_interaction_get_n_flv (process%hi)
  end function process_get_n_flv  

  subroutine process_get_beam_index (process, index)
    type(process_t), intent(in) :: process
    integer, dimension(:), allocatable, intent(out) :: index
    allocate (index (size (process%j_beam)))
    index = process%j_beam
  end subroutine process_get_beam_index

  subroutine process_get_incoming_parton_index (process, index)
    type(process_t), intent(in) :: process
    integer, dimension(:), allocatable, intent(out) :: index
    allocate (index (size (process%j_in)))
    index = process%j_in
  end subroutine process_get_incoming_parton_index

  subroutine process_get_outgoing_parton_index (process, index)
    type(process_t), intent(in) :: process
    integer, dimension(:), allocatable, intent(out) :: index
    allocate (index (size (process%j_out)))
    index = process%j_out
  end subroutine process_get_outgoing_parton_index

  function process_get_beam_flv (process) result (flv_in)
    type(flavor_t), dimension(:), allocatable :: flv_in
    type(process_t), intent(in) :: process
    allocate (flv_in (process_get_n_in (process)))
    if (process%beam_data%initialized)  flv_in = process%beam_data%flv
  end function process_get_beam_flv

  function process_get_beam_energy (process) result (energy)
    real(default), dimension(:), allocatable :: energy
    type(process_t), intent(in) :: process
    allocate (energy (process_get_n_in (process)))
    energy = beam_data_get_energy (process%beam_data)
  end function process_get_beam_energy

  function process_get_n_parameters (process) result (n)
    integer :: n
    type(process_t), intent(in) :: process
    n = process%n_par
  end function process_get_n_parameters

  function process_get_n_channels (process) result (n)
    integer :: n
    type(process_t), intent(in) :: process
    n = process%n_channels
  end function process_get_n_channels

  function process_get_n_bins (process) result (n)
    integer :: n
    type(process_t), intent(in) :: process
    n = process%n_bins
  end function process_get_n_bins

  function process_get_status (process) result (status)
    type(process_status_t) :: status
    type(process_t), intent(in) :: process
    status = process%status
  end function process_get_status

  function process_get_scale (process) result (scale)
    real(default) :: scale
    type(process_t), intent(in) :: process
    scale = process%scale
  end function process_get_scale

  function process_get_fac_scale (process) result (scale)
    real(default) :: scale
    type(process_t), intent(in) :: process
    scale = process%fac_scale
  end function process_get_fac_scale
  
  function process_get_ren_scale (process) result (scale)
    real(default) :: scale
    type(process_t), intent(in) :: process
    scale = process%ren_scale
  end function process_get_ren_scale  

  function process_get_alpha_s (process) result (alpha_s)
    real(default) :: alpha_s
    type(process_t), intent(in) :: process
    alpha_s = process%qcd%alpha_s_at_scale
  end function process_get_alpha_s

  function process_get_sqme (process) result (sqme)
    real(default) :: sqme
    type(process_t), intent(in) :: process
    sqme = process%sqme
  end function process_get_sqme

  function process_get_reweighting_factor (process) result (weight)
    real(default) :: weight
    type(process_t), intent(in) :: process
    weight = process%reweighting_factor
  end function process_get_reweighting_factor

  function process_get_n_calls (process) result (n_calls)
    integer :: n_calls
    type(process_t), intent(in) :: process
    n_calls = integration_results_get_n_calls (process%results)
  end function process_get_n_calls

  function process_get_integral (process) result (integral)
    real(default) :: integral
    type(process_t), intent(in) :: process
    integral = integration_results_get_integral (process%results)
  end function process_get_integral

  function process_get_error (process) result (error)
    real(default) :: error
    type(process_t), intent(in) :: process
    error = integration_results_get_error (process%results)
  end function process_get_error

  function process_get_accuracy (process) result (accuracy)
    real(default) :: accuracy
    type(process_t), intent(in) :: process
    accuracy = integration_results_get_accuracy (process%results)
  end function process_get_accuracy

  function process_get_chi2 (process) result (chi2)
    real(default) :: chi2
    type(process_t), intent(in) :: process
    chi2 = integration_results_get_chi2 (process%results)
  end function process_get_chi2

  function process_get_time_per_event (process) result (tpe)
    real(default) :: tpe
    type(process_t), intent(in) :: process
    tpe = integration_results_get_time_per_event (process%results)
  end function process_get_time_per_event

  function process_get_efficiency (process) result (efficiency)
    real(default) :: efficiency
    type(process_t), intent(in) :: process
    efficiency = integration_results_get_efficiency (process%results)
  end function process_get_efficiency

  function process_get_sample_function_value (process) result  (value)
     real(default) :: value
     type(process_t), intent(in) :: process 
     value = process%sample_function_value
  end function process_get_sample_function_value

  function process_get_current_pass (process) result (pass)
    integer :: pass
    type(process_t), intent(in) :: process
    pass = integration_results_get_current_pass (process%results)
  end function process_get_current_pass

  function process_get_current_it (process) result (it)
    integer :: it
    type(process_t), intent(in) :: process
    it = integration_results_get_current_it (process%results)
  end function process_get_current_it

  function process_get_eval_sqme_ptr (process) result (eval)
    type(evaluator_t), pointer :: eval
    type(process_t), intent(in), target :: process
    if (process%has_extra_evaluators) then
       eval => process%eval_sqme
    else
       eval => hard_interaction_get_eval_sqme_ptr (process%hi)
    end if
  end function process_get_eval_sqme_ptr

  function process_get_eval_flows_ptr (process) result (eval)
    type(evaluator_t), pointer :: eval
    type(process_t), intent(in), target :: process
    if (process%has_extra_evaluators) then
       eval => process%eval_flows
    else
       eval => hard_interaction_get_eval_flows_ptr (process%hi)
    end if
  end function process_get_eval_flows_ptr

  function process_get_hi_int_ptr (process) result (int)
    type(interaction_t), pointer :: int
    type(process_t), intent(in), target :: process
    int => hard_interaction_get_int_ptr (process%hi)
  end function process_get_hi_int_ptr

  function process_get_hi_eval_sqme_ptr (process) result (eval)
    type(evaluator_t), pointer :: eval
    type(process_t), intent(in), target :: process
    eval => hard_interaction_get_eval_sqme_ptr (process%hi)
  end function process_get_hi_eval_sqme_ptr

  function process_get_hi_eval_flows_ptr (process) result (eval)
    type(evaluator_t), pointer :: eval
    type(process_t), intent(in), target :: process
    eval => hard_interaction_get_eval_flows_ptr (process%hi)
  end function process_get_hi_eval_flows_ptr

  subroutine process_mark_as_cascade_decay (process)
    type(process_t), intent(inout) :: process
    process%is_cascade_decay = .true.
  end subroutine process_mark_as_cascade_decay
    
  subroutine process_set_scale (process, scale)
    type(process_t), intent(inout) :: process
    real(default), intent(in) :: scale
    process%scale = scale
  end subroutine process_set_scale

  subroutine process_set_fac_scale (process, scale)
    type(process_t), intent(inout) :: process
    real(default), intent(in) :: scale
    process%fac_scale = scale
  end subroutine process_set_fac_scale

  subroutine process_set_ren_scale (process, scale)
    type(process_t), intent(inout) :: process
    real(default), intent(in) :: scale
    process%ren_scale = scale
  end subroutine process_set_ren_scale

  subroutine process_set_alpha_s (process, alpha_s)
    type(process_t), intent(inout) :: process
    real(default), intent(in) :: alpha_s
    process%qcd%alpha_s_at_scale = alpha_s
  end subroutine process_set_alpha_s

  subroutine process_set_sqme (process, sqme)
    type(process_t), intent(inout) :: process
    real(default), intent(in) :: sqme
    process%sqme = sqme
  end subroutine process_set_sqme

  subroutine process_setup_beams &
       (process, beam_data, n_strfun, n_mapping, sqrts, flv)
    type(process_t), intent(inout), target :: process
    type(beam_data_t), intent(in) :: beam_data
    integer, intent(in) :: n_strfun, n_mapping
    real(default), intent(in), optional :: sqrts
    type(flavor_t), dimension(:), intent(in), optional :: flv
    if (.not. process_has_matrix_element (process))  return
    if (process%use_beams) then
       process%beam_data = beam_data
       process%sqrts = beam_data%sqrts
       process%sqrts_known = .true.
       process%n_strfun = n_strfun
       process%azimuthal_dependence = &
            .not. all (polarization_is_diagonal (beam_data%pol))
       process%lab_is_cm_frame = beam_data%lab_is_cm_frame .and. n_strfun == 0
       call strfun_chain_init (process%sfchain, beam_data, n_strfun, n_mapping)
    else
       select case (process%type)
       case (PRC_DECAY)
          process%sqrts = process%mass_in(1)
          call beam_data_init_decay (process%beam_data, process%flv_in)
       case (PRC_SCATTERING)
          if (present (sqrts)) then
             process%sqrts = sqrts
             call beam_data_init_sqrts &
                  (process%beam_data, process%sqrts, process%flv_in)
          else
             call msg_fatal ("Process setup: neither beams nor sqrts are known")
             process%sqrts = 0
          end if
       end select
       process%sqrts_known = .true.
    end if
  end subroutine process_setup_beams

  subroutine process_set_beam_momenta (process, p)
    type(process_t), intent(inout), target :: process
    type(vector4_t), dimension(:), intent(in) :: p
    type(interaction_t), pointer :: hi_int
    if (.not. process_has_matrix_element (process))  return
    if (process%use_beams) then
       call strfun_chain_set_beam_momenta (process%sfchain, p)
    else
       hi_int => hard_interaction_get_int_ptr (process%hi)
       call interaction_set_momenta (hi_int, p, outgoing=.false.)
    end if
    process%sqrts_hat = process%sqrts
    process%lab_is_cm_frame = .false.
    process%beams_are_set = .true.
  end subroutine process_set_beam_momenta

  subroutine process_set_strfun_lhapdf &
       (process, i, line, lhapdf_data, n_parameters)
    type(process_t), intent(inout), target :: process
    integer, intent(in) :: i, line, n_parameters
    type(lhapdf_data_t), intent(in) :: lhapdf_data
    if (process%use_beams) then
       call strfun_chain_set_strfun &
            (process%sfchain, i, line, lhapdf_data, n_parameters)
    end if
  end subroutine process_set_strfun_lhapdf

  subroutine process_set_strfun_pdf_builtin &
       (process, i, line, pdf_builtin_data, n_parameters)
    type(process_t), intent(inout), target :: process
    integer, intent(in) :: i, line, n_parameters
    type(pdf_builtin_data_t), intent(in) :: pdf_builtin_data
    if (process%use_beams) then
       call strfun_chain_set_strfun &
            (process%sfchain, i, line, pdf_builtin_data, n_parameters)
    end if
  end subroutine process_set_strfun_pdf_builtin

  subroutine process_set_strfun_isr &
       (process, i, line, isr_data, n_parameters)
    type(process_t), intent(inout), target :: process
    integer, intent(in) :: i, line, n_parameters
    type(isr_data_t), intent(in) :: isr_data
    if (process%use_beams) then
       call strfun_chain_set_strfun &
            (process%sfchain, i, line, isr_data, n_parameters)
    end if
  end subroutine process_set_strfun_isr

  subroutine process_set_strfun_epa &
       (process, i, line, epa_data, n_parameters)
    type(process_t), intent(inout), target :: process
    integer, intent(in) :: i, line, n_parameters
!    type(epa_data_t), dimension(:), intent(in) :: epa_data
    type(epa_data_t), intent(in) :: epa_data
    if (process%use_beams) then
       call strfun_chain_set_strfun &
            (process%sfchain, i, line, epa_data, n_parameters)
    end if
  end subroutine process_set_strfun_epa

  subroutine process_set_strfun_ewa &
       (process, i, line, ewa_data, n_parameters)
    type(process_t), intent(inout), target :: process
    integer, intent(in) :: i, line, n_parameters
!    type(ewa_data_t), dimension(:), intent(in) :: ewa_data
    type(ewa_data_t), intent(inout) :: ewa_data
    integer :: k
    integer, dimension(process_get_n_tot(process), &
                       process_get_n_flv(process)) :: flvs_tot
    integer, dimension(process_get_n_flv(process)) :: flvs                     
    flvs_tot = hard_interaction_get_flv_states (process%hi)
    flvs(:) = abs(flvs_tot (line,:))
    do k = 1, size (flvs)
      if (flvs(1) /= flvs (k)) &
        call msg_fatal ("EWA approximation is not applicable when " &
                 // "mixing W and Z for a single beam.")
    end do
    if (flvs(1) < 23 .or. flvs(1) > 24) &
         call msg_fatal ("Hard scattering process does not match EWA.") 
    if (process%use_beams) then
       call strfun_chain_set_strfun &
            (process%sfchain, i, line, ewa_data, n_parameters, flvs(1))
    end if
  end subroutine process_set_strfun_ewa

  subroutine process_set_strfun_circe1 &
       (process, i, line, circe1_data, n_parameters)
    type(process_t), intent(inout), target :: process
    integer, intent(in) :: i, line, n_parameters
    type(circe1_data_t), intent(in) :: circe1_data
    if (process%use_beams) then
       call strfun_chain_set_strfun &
            (process%sfchain, i, line, circe1_data, n_parameters)
    end if
  end subroutine process_set_strfun_circe1

  subroutine process_set_strfun_circe2 &
       (process, i, line, circe2_data, n_parameters)
    type(process_t), intent(inout), target :: process
    integer, intent(in) :: i, line, n_parameters
    type(circe2_data_t), intent(in) :: circe2_data
    if (process%use_beams) then
       call strfun_chain_set_strfun &
            (process%sfchain, i, line, circe2_data, n_parameters)
    end if
  end subroutine process_set_strfun_circe2

  subroutine process_set_strfun_escan &
       (process, i, line, escan_data, n_parameters)
    type(process_t), intent(inout), target :: process
    integer, intent(in) :: i, line, n_parameters
    type(escan_data_t), intent(in) :: escan_data
    if (process%use_beams) then
       call strfun_chain_set_strfun &
            (process%sfchain, i, line, escan_data, n_parameters)
    end if
  end subroutine process_set_strfun_escan

  subroutine process_set_strfun_beam_events &
       (process, i, line, beam_events_data, n_parameters)
    type(process_t), intent(inout), target :: process
    integer, intent(in) :: i, line, n_parameters
    type(beam_events_data_t), intent(in) :: beam_events_data
    if (process%use_beams) then
       call strfun_chain_set_strfun &
            (process%sfchain, i, line, beam_events_data, n_parameters)
    end if
  end subroutine process_set_strfun_beam_events

  subroutine process_set_strfun_user &
       (process, i, line, user_data, n_parameters)
    type(process_t), intent(inout), target :: process
    integer, intent(in) :: i, line, n_parameters
    type(sf_user_data_t), intent(in) :: user_data
    if (process%use_beams) then
       call strfun_chain_set_strfun &
            (process%sfchain, i, line, user_data, n_parameters)
    end if
  end subroutine process_set_strfun_user

  subroutine process_set_strfun_mapping (process, i, index, type, par)
    type(process_t), intent(inout) :: process
    integer, intent(in) :: i
    integer, intent(in) :: type
    integer, dimension(:), intent(in) :: index
    real(default), dimension(:), intent(in) :: par
    if (process%use_beams) then
       call strfun_chain_set_mapping (process%sfchain, i, index, type, par)
    end if
  end subroutine process_set_strfun_mapping

  subroutine process_allow_global_mapping (process)
    type(process_t), intent(inout) :: process
    process%allow_s_channel_mapping = .true.
  end subroutine process_allow_global_mapping

  subroutine process_connect_strfun (process, ok)
    type(process_t), intent(inout), target :: process
    logical, intent(out), optional :: ok
    integer, dimension(:), allocatable :: coll_index
    type(quantum_numbers_mask_t), dimension(:), allocatable :: mask_in
    type(quantum_numbers_mask_t) :: mask_tr
    type(evaluator_t), pointer :: eval_sfchain, eval_hi
    type(interaction_t), pointer :: int_beam, int_hi
    integer :: n_in, i
    if (.not. process_has_matrix_element (process))  return
    n_in  = hard_interaction_get_n_in  (process%hi)
    allocate (mask_in (n_in))
    if (process%use_beams) then
       call strfun_chain_make_evaluators (process%sfchain, ok)
       allocate (coll_index (n_in))
       coll_index = strfun_chain_get_colliding_particles (process%sfchain)
       mask_in = strfun_chain_get_colliding_particles_mask (process%sfchain)
    else
       mask_in = new_quantum_numbers_mask (.true., .true., .true.)
    end if
    call hard_interaction_init_trace &
         (process%hi, mask_in, process%use_hi_color_factors)
    if (process%use_beams) then
       int_beam => strfun_chain_get_beam_int_ptr (process%sfchain)
       eval_sfchain => strfun_chain_get_last_evaluator_ptr (process%sfchain)
       eval_hi => hard_interaction_get_eval_trace_ptr (process%hi)
       int_hi => hard_interaction_get_int_ptr (process%hi)
       mask_tr = new_quantum_numbers_mask (.true., .true., .true.)
       if (associated (eval_sfchain)) then
          do i = 1, n_in
             call interaction_set_source_link &
                  (int_hi, i, eval_sfchain, coll_index(i))
             call evaluator_set_source_link &
                  (eval_hi, i, eval_sfchain, coll_index(i))
          end do
          call evaluator_init_product &
               (process%eval_trace, eval_sfchain, eval_hi, mask_tr, mask_tr)
          if (evaluator_is_empty (process%eval_trace)) then
             call msg_fatal ("Mismatch between structure functions and hard process")
             if (present (ok))  ok = .false.
             return
          end if
       else
          do i = 1, n_in
             call interaction_set_source_link &
                  (int_hi, i, int_beam, coll_index(i))
             call evaluator_set_source_link &
                  (eval_hi, i, int_beam, coll_index(i))
          end do
          call evaluator_init_product &
               (process%eval_trace, int_beam, eval_hi, mask_tr, mask_tr)
          if (evaluator_is_empty (process%eval_trace)) then
             call msg_fatal ("Mismatch between beams and hard process")
             if (present (ok))  ok = .false.
             return
          end if
       end if
       process%n_par_strfun = &
            strfun_chain_get_n_parameters_tot (process%sfchain)
       allocate (process%x_strfun (process%n_par_strfun))
    end if
    process%n_par = process%n_par_strfun + process%n_par_hi
    if (present (ok))  ok = .true.
  end subroutine process_connect_strfun

  subroutine process_check_beam_setup (process, var_list)
    type(process_t), intent(in) :: process
    type(var_list_t), intent(in) :: var_list
    logical :: sqrts_known
    real(default) :: sqrts
    sqrts_known = var_list_is_known (var_list, "sqrts")
    sqrts = var_list_get_rval (var_list, "sqrts")
    if (process%use_beams) then
       select case (process%type)
       case (PRC_SCATTERING)
          if (sqrts_known) then
             call beam_data_check_scattering (process%beam_data, sqrts)
          else
             call beam_data_check_scattering (process%beam_data)
          end if
       end select
    end if
  end subroutine process_check_beam_setup

  subroutine process_setup_phase_space (process, rebuild_phs, &  
       os_data, phs_par, mapping_defaults, filename_out, &
       filename_in, filename_vis, vis_channels, ok)
    type(process_t), intent(inout), target :: process
    logical, intent(in) :: rebuild_phs
    type(os_data_t), intent(in) :: os_data
    type(phs_parameters_t), intent(inout) :: phs_par
    type(mapping_defaults_t), intent(in) :: mapping_defaults
    type(string_t), intent(in), optional :: &
       filename_out, filename_in, filename_vis
    logical, intent(in) :: vis_channels
    logical, intent(out), optional :: ok
    type(string_t) :: filename, setenv_tex, setenv_mp, &
       pipe, pipe_dvi
    logical :: exist, check
    integer :: extra_off_shell
    type(cascade_set_t) :: cascade_set
    logical :: variable_limits
    integer :: n_in, n_out, n_tot, n_flv
    type(flavor_t), dimension(:,:), allocatable :: flv
    integer :: n_par_strfun
    logical, dimension(:), allocatable :: strfun_rigid
    character(32) :: md5sum_process, md5sum_model, md5sum_parameters
    integer :: unit, unit_tex, unit_dev, status
    logical :: phs_ok, phs_match, wrote_file
    phs_ok = .false.
    phs_match = .false.
    variable_limits = process%n_strfun /= 0
    n_in  = hard_interaction_get_n_in  (process%hi)
    n_out = hard_interaction_get_n_out (process%hi)
    n_tot = hard_interaction_get_n_tot (process%hi)
    n_flv = hard_interaction_get_n_flv (process%hi)
    allocate (flv (n_tot, n_flv))
    call flavor_init (flv, &
         hard_interaction_get_flv_states (process%hi), process%model)
    md5sum_process = process%md5sum
    md5sum_model = model_get_md5sum (process%model)
    md5sum_parameters = model_get_parameters_md5sum (process%model)        
    phs_par%sqrts = process%sqrts
    if (present (filename_in)) then
       filename = filename_in
       call msg_message ("Reading phase-space configuration from file '" &
            // char (filename) // "'")
       check = .false.
    else if (.not. rebuild_phs .and. present (filename_out)) then
       filename = filename_out
       check = .true.
    else
       filename = ""
    end if
    if (filename /= "") then
       inquire (file=char(filename), exist=exist)
       if (exist) then
          if (check) then
             call phs_forest_read (process%forest, filename, &
                  process%id, n_in, n_out, process%model, phs_ok, &
                  md5sum_process, md5sum_model, md5sum_parameters, phs_par, &
                  phs_match)
          else
             call phs_forest_read (process%forest, filename, &
                  process%id, n_in, n_out, process%model, phs_ok)
             phs_match = .true.
          end if
          if (phs_match) &
               call msg_message ("Read phase-space configuration from file '" &
               // char (filename) // "'...")
          unit = free_unit ()
          open (unit = unit, file = char (filename), action = "read", &
                status = "old")
          process%md5sum_phs = md5sum (unit)
          close (unit)
          if (.not. phs_ok) then
             call msg_fatal ("Phase space file '" // char (filename) &
                  // "': No valid phase space for process '" &
                  // char (process%id) // "'")
             if (present (ok))  ok = .false.
             return
          end if
       else
          call msg_message ("Phase space file '" // char (filename) &
               // "' not found.")
          phs_match = .false.
       end if
    end if
    wrote_file = .false.
    if (.not. phs_match) then
       call msg_message ("Generating phase space configuration ...")
       LOOP_OFF_SHELL: do extra_off_shell = 0, max (n_tot - 3, 0)
          call cascade_set_generate (cascade_set, &
               process%model, n_in, n_out, flv, phs_par, process%fatal_beam_decay)
          if (cascade_set_is_valid (cascade_set)) then
             exit LOOP_OFF_SHELL
          else if (phs_par%off_shell >= max (n_tot - 3, 0)) then
             call msg_error ("Process '" // char (process%id) &
                  // "': no valid phase-space channels found")
             if (present (ok))  ok = .false.
             call cascade_set_final (cascade_set)
             return
          else
             write (msg_buffer, "(A,1x,I0)") &
                  "Process '" // char (process%id) &
                  // "': no valid phase-space channels found for " &
                  // "phs_off_shell =", phs_par%off_shell
             call msg_warning ()
             call msg_message ("Increasing phs_off_shell")
             phs_par%off_shell = phs_par%off_shell + 1
          end if
       end do LOOP_OFF_SHELL
       unit = free_unit ()
       if (present (filename_out)) then
          open (unit, file=char(filename_out), &
                action="readwrite", status="replace")
       else
          open (unit, action="readwrite", status="scratch")
       end if
       write (unit, *) "process ", char (process%id)
       write (unit, *)
       call cascade_set_write_process_bincode_format (cascade_set, unit)
       write (unit, *)
       write (unit, *) "  md5sum_process    = ", '"', md5sum_process, '"'
       write (unit, *) "  md5sum_model      = ", '"', md5sum_model, '"'
       write (unit, *) "  md5sum_parameters = ", '"', md5sum_parameters, '"'
       call phs_parameters_write (phs_par, unit)
       call cascade_set_write_file_format (cascade_set, unit)
       if (vis_channels) then 
         unit_tex = free_unit ()
         open (unit=unit_tex, file=char(filename_vis // ".tex"), &
           action="write", status="replace")      
         call cascade_set_write_graph_format (cascade_set, &
            filename_vis // ".graphs", process_get_id (process), unit_tex)
         close (unit_tex)      
         call msg_message ("Writing visualized phase space channels file " & 
            // char(trim(filename_vis)) // "...")        
         if (os_data%event_analysis_ps) then
         BLOCK: do
            unit_dev = free_unit ()
            open (file = "/dev/null", unit = unit_dev, &
                 action = "write", iostat = status)
            if (status /= 0) then
               pipe = ""
               pipe_dvi = ""
            else
               pipe = " > /dev/null"
               pipe_dvi = " 2>/dev/null 1>/dev/null"
            end if
            close (unit_dev)
            if (os_data%whizard_texpath /= "") then
               setenv_tex = &
                  "TEXINPUTS=" // os_data%whizard_texpath // ":$TEXINPUTS "
               setenv_mp = &
                  "MPINPUTS=" // os_data%whizard_texpath // ":$MPINPUTS "
            else
               setenv_tex = ""
               setenv_mp = ""
            end if
            call os_system_call (setenv_tex // os_data%latex // " " // &
               filename_vis // ".tex " // pipe, status)
            if (status /= 0)  exit BLOCK
            if (os_data%mpost /= "") then
               call os_system_call (setenv_mp // os_data%mpost // " " // &
                  filename_vis // ".graphs.mp" // pipe, status)
            else 
               call msg_fatal ("Could not use MetaPOST.")
            end if
            if (status /= 0)  exit BLOCK
            call os_system_call (setenv_tex // os_data%latex // " " // &
                filename_vis // ".tex" // pipe, status)
            if (status /= 0)  exit BLOCK
            call os_system_call (os_data%dvips // " -o " // filename_vis &
               // ".ps " // filename_vis // ".dvi" // pipe_dvi, status)
            if (status /= 0)  exit BLOCK
            if (os_data%event_analysis_pdf) then
               call os_system_call (os_data%ps2pdf // " " // &
                     filename_vis // ".ps", status)
               if (status /= 0)  exit BLOCK
            end if
            exit BLOCK
         end do BLOCK
         if (status /= 0) then
          call msg_error ("Unable to compile analysis output file")
         end if
       end if    
       end if    
       call msg_message ("... done.")           
       call cascade_set_final (cascade_set)
       rewind (unit)
       call phs_forest_read (process%forest, unit, &
            process%id, n_in, n_out, process%model, phs_ok)
       rewind (unit)
       process%md5sum_phs = md5sum (unit)
       close (unit)
       wrote_file = present (filename_out)
       if (.not. phs_ok) then
          call msg_bug ("Generated phase space file: " &
               // "No valid phase space for process '" &
               // char (process%id) // "'")
       end if
    end if
    call phs_forest_set_flavors (process%forest, flv(:,1))
    call phs_forest_set_parameters &
         (process%forest, mapping_defaults, variable_limits)
    call phs_forest_setup_prt_combinations (process%forest)
    call phs_forest_set_equivalences (process%forest)
    if (process%use_beams) then
       n_par_strfun = strfun_chain_get_n_parameters_tot (process%sfchain)
       allocate (strfun_rigid (n_par_strfun))
       strfun_rigid = strfun_chain_dimension_is_rigid (process%sfchain)
    else
       n_par_strfun = 0
       allocate (strfun_rigid (0))
    end if
    call phs_forest_setup_vamp_equivalences (process%forest, &
         n_par_strfun, strfun_rigid, &
         process%azimuthal_dependence, &
         process%vamp_eq)
    process%n_channels = phs_forest_get_n_channels (process%forest)
    process%n_par_hi = phs_forest_get_n_parameters (process%forest)
    process%n_par = process%n_par_strfun + process%n_par_hi
    allocate (process%x_hi (process%n_par_hi))
    process%x_hi = 0
    allocate (process%x (process%n_par, process%n_channels))
    process%x = 0
    allocate (process%phs_factor (process%n_channels))
    process%phs_factor = 0
    allocate (process%active_channel (process%n_channels))
    process%active_channel = .true.
    call phs_forest_set_global_mappings (process%forest)
    write (msg_buffer, "(A,I0,A,I0,A)")  "... found ", process%n_channels, &
         " phase space channels, collected in ", &
         phs_forest_get_n_groves (process%forest), &
         " groves."
    call msg_message ()
    write (msg_buffer, "(A,I0,A)")  "Phase space: found ", &
         phs_forest_get_n_equivalences (process%forest), &
         " equivalences between channels."
    call msg_message ()
    if (wrote_file) &
         call msg_message ("Wrote phase-space configuration file '" &
         // char (filename_out) // "'.")
    if (present (ok))  ok = .true.
  end subroutine process_setup_phase_space

  subroutine process_setup_subevt (process)
    type(process_t), intent(inout), target :: process
    type(interaction_t), pointer :: int
    integer :: n_beam, n_in, n_out
    integer :: i
    if (process%use_beams) then
       int => evaluator_get_int_ptr (process%eval_trace)
    else
       int => hard_interaction_get_int_ptr (process%hi)
    end if
    n_beam = size (process%j_beam)
    n_in = size (process%j_in)
    n_out = size (process%j_out)
    process%j_beam = (/ (i, i = 1, n_beam) /)
    process%j_in = (/ (i + strfun_chain_get_n_vir (process%sfchain), &
                       i = 1, n_in) /)
    process%j_out = interaction_get_children (int, process%j_in(1))
    call interaction_to_subevt (int, &
         process%j_beam, process%j_in, process%j_out, process%subevt)
    call subevt_set_pdg_beam (process%subevt, &
         flavor_get_pdg (beam_data_get_flavor (process%beam_data)))
    call subevt_set_pdg_incoming (process%subevt, &
         flavor_get_pdg (process%flv_in))
    call subevt_set_pdg_outgoing (process%subevt, &
         flavor_get_pdg (process%flv_out))
  end subroutine process_setup_subevt

  subroutine process_setup_cuts (process, parse_node, md5sum)
    type(process_t), intent(inout), target :: process
    type(parse_node_t), intent(in), optional, target :: parse_node
    character(32), intent(out), optional :: md5sum
    if (present (parse_node)) then
       process%cut_pn => parse_node
       call eval_tree_init_lexpr &
            (process%cut_expr, parse_node, process%var_list, process%subevt)
    else if (associated (process%cut_pn)) then
       call eval_tree_init_lexpr &
            (process%cut_expr, process%cut_pn, process%var_list, process%subevt)
    end if
    if (present (md5sum)) &
         md5sum = eval_tree_get_md5sum (process%cut_expr)
  end subroutine process_setup_cuts

  subroutine process_setup_weight (process, parse_node, md5sum)
    type(process_t), intent(inout), target :: process
    type(parse_node_t), intent(in), optional, target :: parse_node
    character(32), intent(out), optional :: md5sum
    if (present (parse_node)) then
       process%weight_pn => parse_node
       call eval_tree_init_expr &
            (process%reweighting_expr, parse_node, process%var_list, &
            process%subevt)
    else if (associated (process%weight_pn)) then
       call eval_tree_init_expr &
            (process%reweighting_expr, process%weight_pn, process%var_list, &
            process%subevt)
    end if
    if (present (md5sum)) &
         md5sum = eval_tree_get_md5sum (process%reweighting_expr)
  end subroutine process_setup_weight

  subroutine process_setup_scale (process, parse_node, md5sum)
    type(process_t), intent(inout), target :: process
    type(parse_node_t), intent(in), optional, target :: parse_node
    character(32), intent(out), optional :: md5sum
    if (present (parse_node)) then
       process%scale_pn => parse_node
       call eval_tree_init_expr &
            (process%scale_expr, parse_node, process%var_list, process%subevt)
    else if (associated (process%scale_pn)) then
       call eval_tree_init_expr &
            (process%scale_expr, process%scale_pn, process%var_list, &
            process%subevt)
    end if
    if (present (md5sum)) &
         md5sum = eval_tree_get_md5sum (process%scale_expr)
  end subroutine process_setup_scale

  subroutine process_setup_fac_scale (process, parse_node, md5sum)
    type(process_t), intent(inout), target :: process
    type(parse_node_t), intent(in), optional, target :: parse_node
    character(32), intent(out), optional :: md5sum
    if (present (parse_node)) then
       process%fac_scale_pn => parse_node
       call eval_tree_init_expr &
            (process%fac_scale_expr, parse_node, process%var_list, &
            process%subevt)
    else if (associated (process%fac_scale_pn)) then
       call eval_tree_init_expr &
            (process%fac_scale_expr, process%fac_scale_pn, process%var_list, &
            process%subevt)
    end if
    if (present (md5sum)) &
         md5sum = eval_tree_get_md5sum (process%fac_scale_expr)
  end subroutine process_setup_fac_scale

  subroutine process_setup_ren_scale (process, parse_node, md5sum)
    type(process_t), intent(inout), target :: process
    type(parse_node_t), intent(in), optional, target :: parse_node
    character(32), intent(out), optional :: md5sum
    if (present (parse_node)) then
       process%ren_scale_pn => parse_node
       call eval_tree_init_expr &
            (process%ren_scale_expr, parse_node, process%var_list, &
            process%subevt)
    else if (associated (process%ren_scale_pn)) then
       call eval_tree_init_expr &
            (process%ren_scale_expr, process%ren_scale_pn, process%var_list, &
            process%subevt)
    end if
    if (present (md5sum)) &
         md5sum = eval_tree_get_md5sum (process%ren_scale_expr)
  end subroutine process_setup_ren_scale

  subroutine grid_parameters_write (grid_par, unit)
    type(grid_parameters_t), intent(in) :: grid_par
    integer, intent(in), optional :: unit
    integer :: u
    u = output_unit (unit)
    write (u, *) "threshold_calls       = ", grid_par%threshold_calls 
    write (u, *) "min_calls_per_channel = ", grid_par%min_calls_per_channel
    write (u, *) "min_calls_per_bin     = ", grid_par%min_calls_per_bin
    write (u, *) "min_bins              = ", grid_par%min_bins
    write (u, *) "max_bins              = ", grid_par%max_bins
    write (u, *) "stratified            = ", grid_par%stratified
    write (u, *) "use_vamp_equivalences = ", grid_par%use_vamp_equivalences
    write (u, *) "channel_weights_power = ", grid_par%channel_weights_power
  end subroutine grid_parameters_write

  subroutine grid_parameters_read (grid_par, unit)
    type(grid_parameters_t), intent(out) :: grid_par
    integer, intent(in) :: unit
    character(30) :: dummy
    character :: equals
    read (unit, *) dummy, equals, grid_par%threshold_calls 
    read (unit, *) dummy, equals, grid_par%min_calls_per_channel
    read (unit, *) dummy, equals, grid_par%min_calls_per_bin
    read (unit, *) dummy, equals, grid_par%min_bins
    read (unit, *) dummy, equals, grid_par%max_bins
    read (unit, *) dummy, equals, grid_par%stratified
    read (unit, *) dummy, equals, grid_par%use_vamp_equivalences
    read (unit, *) dummy, equals, grid_par%channel_weights_power
  end subroutine grid_parameters_read

  function grid_parameters_eq (gp1, gp2) result (eq)
    logical :: eq
    type(grid_parameters_t), intent(in) :: gp1, gp2
    eq = gp1%threshold_calls == gp2%threshold_calls &
         .and. gp1%min_calls_per_channel    == gp2%min_calls_per_channel &
         .and. gp1%min_calls_per_bin        == gp2%min_calls_per_bin     &
         .and. gp1%min_bins                 == gp2%min_bins              &
         .and. gp1%max_bins                 == gp2%max_bins              &
         .and.(gp1%stratified            .eqv. gp2%stratified           )&
         .and.(gp1%use_vamp_equivalences .eqv. gp2%use_vamp_equivalences)&
         .and. gp1%channel_weights_power    == gp2%channel_weights_power
  end function grid_parameters_eq

  function grid_parameters_ne (gp1, gp2) result (ne)
    logical :: ne
    type(grid_parameters_t), intent(in) :: gp1, gp2
    ne = gp1%threshold_calls /= gp2%threshold_calls &
         .or. gp1%min_calls_per_channel     /= gp2%min_calls_per_channel &
         .or. gp1%min_calls_per_bin         /= gp2%min_calls_per_bin     &
         .or. gp1%min_bins                  /= gp2%min_bins              &
         .or. gp1%max_bins                  /= gp2%max_bins              &
         .or.(gp1%stratified            .neqv. gp2%stratified           )&
         .or.(gp1%use_vamp_equivalences .neqv. gp2%use_vamp_equivalences)&
         .or. gp1%channel_weights_power     /= gp2%channel_weights_power
  end function grid_parameters_ne

  subroutine process_setup_grids (process, grid_parameters, calls)
    type(process_t), intent(inout), target :: process
    type(grid_parameters_t), intent(in) :: grid_parameters
    integer, intent(in) :: calls
    integer, dimension(:), allocatable :: num_div
    real(default), dimension(:), allocatable :: weights
    real(default), dimension(:,:), allocatable :: region
    integer :: min_calls
    allocate (num_div (process%n_par))
    min_calls = grid_parameters%min_calls_per_bin * process%n_channels
    if (min_calls /= 0) then
       process%n_bins =  max (grid_parameters%min_bins, &
            min (calls / min_calls, grid_parameters%max_bins))
    else
       process%n_bins = grid_parameters%max_bins
    end if
    allocate (region (2, process%n_par))
    region(1,:) = 0
    region(2,:) = 1
    allocate (weights (process%n_channels))
    weights = 1
    num_div = process%n_bins
    call msg_message ("Creating VAMP integration grids:")
    if (grid_parameters%use_vamp_equivalences) &
         call msg_message ("Using phase-space channel equivalences.")
    call vamp_create_grids (process%grids, region, calls, weights, &
         num_div=num_div, stratified=grid_parameters%stratified)
    process%vamp_grids_defined = .true.
  end subroutine process_setup_grids

  subroutine process_reset_helicity_selection (process, threshold, cutoff)
    type(process_t), intent(inout) :: process
    real(default), intent(in) :: threshold
    integer, intent(in) :: cutoff
    call hard_interaction_reset_helicity_selection &
         (process%hi, threshold, cutoff)
  end subroutine process_reset_helicity_selection

  subroutine process_set_kinematics (process, x_in, channel, ok)
    type(process_t), intent(inout), target :: process
    real(default), dimension(:), intent(in) :: x_in
    integer, intent(in) :: channel
    logical, intent(out) :: ok
    type(interaction_t), pointer :: int
    type(evaluator_t), pointer :: eval
    integer :: i
    real(default) :: lda
    process%x_hi = x_in(:process%n_par_hi)
    process%x_strfun = x_in(process%n_par_hi+1:)
!     process%x_strfun = x_in(:process%n_par_strfun)
!     process%x_hi = x_in(process%n_par_strfun+1:)
    process%channel = channel
    int => hard_interaction_get_int_ptr (process%hi)
    eval => hard_interaction_get_eval_trace_ptr (process%hi)
    if (process%use_beams) then
       if (process%allow_s_channel_mapping) then
          call strfun_chain_set_kinematics (process%sfchain, process%x_strfun, &
               phs_forest_tree_has_global_mapping (process%forest, channel), &
               ok)
       else
          call strfun_chain_set_kinematics (process%sfchain, process%x_strfun, &
               ok=ok)
       end if
       if (.not. ok)  return
       call interaction_receive_momenta (int)
       process%beams_are_set = .true.
       process%sqrts_hat = sqrt (max (interaction_get_s (int), 0._default))
    else if (.not. process%beams_are_set) then
       select case (process%type)
       case (PRC_DECAY)
          call interaction_set_momenta (int, &
               (/ vector4_at_rest (process%mass_in(1)) /), &
               outgoing=.false.)
       case (PRC_SCATTERING)
          call interaction_set_momenta (int, &
               colliding_momenta (process%sqrts, process%mass_in), &
               outgoing=.false.)
       end select
       process%sqrts_hat = process%sqrts
    end if
    call process_status_passed_strfun_chain (process%status)
    select case (process%type)
    case (PRC_DECAY)
       process%flux_factor = &
            twopi4 / (2 * process%mass_in(1))
    case (PRC_SCATTERING)
       lda = lambda (process%sqrts_hat ** 2, &
                                 process%mass_in(1) ** 2, &
                                 process%mass_in(2) ** 2)
       if (lda <= 0) then
          ok = .false.; return
       end if
       process%flux_factor = &
            conv * twopi4 / (2 * sqrt (lda))
    end select
    call process_status_passed_mass_threshold (process%status)
    process%sqrts_hat_known = .true.
    if (.not. process%lab_is_cm_frame) then
       process%lt_cm_to_lab = interaction_get_cm_transformation (int)
       call phs_forest_set_prt_in (process%forest, int, process%lt_cm_to_lab)
    else
       call phs_forest_set_prt_in (process%forest, int)
    end if
    process%x(:process%n_par_hi,channel) = process%x_hi
    forall (i = 1 : process%n_par_strfun)
       process%x(process%n_par_hi+i,:) = process%x_strfun(i)
    end forall
    if (process%old_phs_version) then
       call phs_forest_evaluate_phase_space (process%forest, &
            channel, process%active_channel, process%sqrts_hat, &
            process%x, process%phs_factor, process%phs_volume, ok)
    else
       call phs_forest_evaluate_momenta (process%forest, &
            channel, process%active_channel, process%sqrts_hat, &
            process%x, process%phs_factor, process%phs_volume, ok)
    end if
    if (.not. ok)  return
    if (process%lab_is_cm_frame) then
       call phs_forest_get_prt_out (process%forest, int)
    else
       call phs_forest_get_prt_out &
            (process%forest, int, process%lt_cm_to_lab)
    end if
    call evaluator_receive_momenta (eval)
    if (process%use_beams) &
         call evaluator_receive_momenta (process%eval_trace)
    call process_status_passed_kinematics (process%status)
  end subroutine process_set_kinematics

  subroutine process_complete_kinematics (process, channel)
    type(process_t), intent(inout), target :: process
    integer, intent(in) :: channel
    if (.not. process%old_phs_version) then
       call phs_forest_evaluate_other_channels (process%forest, &
            channel, process%active_channel, process%sqrts_hat, &
            process%x, process%phs_factor)
    end if
  end subroutine process_complete_kinematics

  subroutine process_recover_kinematics (process, particle_set)
    type(process_t), intent(inout), target :: process
    type(particle_set_t), intent(in) :: particle_set
    integer :: n_in, n_out
    real(default) :: lda
    type(evaluator_t), pointer :: eval
    type(interaction_t), pointer :: int
! To be implemented later
    if (process%use_beams) &
         call msg_bug ("Recovering process with beams not implemented yet")

    call hard_interaction_recover_kinematics (process%hi, particle_set)
    int => hard_interaction_get_int_ptr (process%hi)

    process%sqrts_hat = process%sqrts
    select case (process%type)
    case (PRC_DECAY)
       process%flux_factor = &
            twopi4 / (2 * process%mass_in(1))
    case (PRC_SCATTERING)
       lda = lambda (process%sqrts_hat ** 2, &
                                 process%mass_in(1) ** 2, &
                                 process%mass_in(2) ** 2)
       if (lda <= 0) then
          process%flux_factor = 0
       else
          process%flux_factor = &
               conv * twopi4 / (2 * sqrt (lda))
       end if
    end select
    process%sqrts_hat_known = .true.
    if (.not. process%lab_is_cm_frame) then
       process%lt_cm_to_lab = interaction_get_cm_transformation (int)
       call phs_forest_set_prt_in (process%forest, int, process%lt_cm_to_lab)
    else
       call phs_forest_set_prt_in (process%forest, int)
    end if

    eval => hard_interaction_get_eval_trace_ptr (process%hi)
    call evaluator_receive_momenta (eval)
    
  end subroutine process_recover_kinematics

  subroutine process_fill_subevt (process, transform)
    type(process_t), intent(inout), target :: process
    logical, intent(in), optional :: transform
    type(interaction_t), pointer :: int
    logical :: tr
    tr = .false.;  if (present (transform))  tr = transform
    if (process%use_beams) then
       int => evaluator_get_int_ptr (process%eval_trace)
    else
       int => hard_interaction_get_int_ptr (process%hi)
    end if
    if (tr) then
       call interaction_momenta_to_subevt &
            (int, process%j_beam, process%j_in, process%j_out, &
             inverse (process%lt_cm_to_lab), process%subevt)
    else
       call interaction_momenta_to_subevt &
            (int, process%j_beam, process%j_in, process%j_out, process%subevt)
    end if
  end subroutine process_fill_subevt

  function process_passes_cuts (process) result (flag)
    logical :: flag
    type(process_t), intent(inout), target :: process
    if (eval_tree_is_defined (process%cut_expr)) then
       call eval_tree_evaluate (process%cut_expr)
       if (eval_tree_result_is_known (process%cut_expr)) then
          flag = eval_tree_get_log (process%cut_expr)
       else
          flag = .true.
       end if
    else
       flag = .true.
    end if
  end function process_passes_cuts

  subroutine process_compute_reweighting_factor (process)
    type(process_t), intent(inout), target :: process
    if (eval_tree_is_defined (process%reweighting_expr)) then
       call eval_tree_evaluate (process%reweighting_expr)
       if (eval_tree_result_is_known (process%reweighting_expr)) then
          process%reweighting_factor = &
               eval_tree_get_real (process%reweighting_expr)
       else
          process%reweighting_factor = 1
       end if
    else
       process%reweighting_factor = 1
    end if
  end subroutine process_compute_reweighting_factor

  subroutine process_compute_scale (process)
    type(process_t), intent(inout), target :: process
    if (eval_tree_is_defined (process%scale_expr)) then
       call eval_tree_evaluate (process%scale_expr)
       if (eval_tree_result_is_known (process%scale_expr)) then
          process%scale = eval_tree_get_real (process%scale_expr)
       else
          process%scale = process%sqrts_hat
       end if
    else
       process%scale = process%sqrts_hat      
    end if   
    if (eval_tree_is_defined (process%fac_scale_expr)) then
       call eval_tree_evaluate (process%fac_scale_expr)
       if (eval_tree_result_is_known (process%fac_scale_expr)) then
          process%fac_scale = eval_tree_get_real (process%fac_scale_expr)
       else
          process%fac_scale = process%scale
       end if
    else
       process%fac_scale = process%scale
    end if
    if (eval_tree_is_defined (process%ren_scale_expr)) then
       call eval_tree_evaluate (process%ren_scale_expr)
       if (eval_tree_result_is_known (process%ren_scale_expr)) then
          process%ren_scale = eval_tree_get_real (process%ren_scale_expr)
       else
          process%ren_scale = process%scale
       end if
    else
       process%ren_scale = process%scale
    end if    
  end subroutine process_compute_scale

  subroutine process_compute_vamp_phs_factor (process, weights)
    type(process_t), intent(inout), target :: process
    real(default), dimension(:), intent(in) :: weights
    real(default), dimension(process%n_channels) :: vamp_prob
    real(default) :: dp
    integer :: i
    !$OMP PARALLEL PRIVATE(i) SHARED(process,vamp_prob)
    !$OMP DO
    do i = 1, process%n_channels
       if (process%active_channel(i)) then
          vamp_prob(i) = &
               vamp_probability (process%grids%grids(i), process%x(:,i))
       else
          vamp_prob(i) = 0
       end if
    end do
    !$OMP END DO
    !$OMP END PARALLEL
    dp = dot_product (weights, vamp_prob / process%phs_factor)
    if (dp /= 0) then
       process%vamp_phs_factor = vamp_prob(process%channel) / dp
    else
       process%vamp_phs_factor = 0
    end if
  end subroutine process_compute_vamp_phs_factor

  subroutine process_update_parameters (process)
    type(process_t), intent(inout) :: process
    call hard_interaction_update_parameters (process%hi)
  end subroutine process_update_parameters

  subroutine process_update_alpha_s (process)
    type(process_t), intent(inout) :: process
    if (.not. process%qcd%alpha_s_is_fixed) then
       call qcd_parameters_update_alpha_s (process%qcd, process%ren_scale)
       call hard_interaction_update_alpha_s &
            (process%hi, process%qcd%alpha_s_at_scale)
    end if
  end subroutine process_update_alpha_s

  subroutine process_evaluate (process)
    type(process_t), intent(inout), target :: process
    if (process%use_beams) then
       call strfun_chain_evaluate (process%sfchain, process%fac_scale)
       process%sf_mapping_factor = &
            strfun_chain_get_mapping_factor (process%sfchain)
    else
       process%sf_mapping_factor = 1
    end if
    call hard_interaction_evaluate (process%hi)
    if (process%has_extra_evaluators) then
       call evaluator_evaluate (process%eval_trace)
       process%sqme = evaluator_sum (process%eval_trace)
    else
       process%sqme = evaluator_sum &
            (hard_interaction_get_eval_trace_ptr (process%hi)) &
            * process%averaging_factor
    end if
!    call process_write (process, 66); stop
  end subroutine process_evaluate

  function process_compute_sqme_sum (process, p) result (sqme)
    real(default) :: sqme
    type(process_t), intent(inout), target :: process
    type(vector4_t), dimension(:), intent(in) :: p
    sqme = hard_interaction_compute_sqme_sum (process%hi, p)
  end function process_compute_sqme_sum

  function process_get_vamp_efficiency_array (process) result (efficiency)
    real(default), dimension(:), allocatable :: efficiency
    type(process_t), intent(in) :: process
    allocate (efficiency (process%n_channels))
    where (process%grids%grids%f_max /= 0)
       efficiency = process%grids%grids%mu(1) / abs (process%grids%grids%f_max)        
    elsewhere
       efficiency = 0
    end where
  end function process_get_vamp_efficiency_array

  function process_get_vamp_efficiency (process) result (efficiency)
    real(default) :: efficiency
    type(process_t), intent(in) :: process
    real(default), dimension(:), allocatable :: weight
    real(default) :: norm
    allocate (weight (process%n_channels))
    weight = process%grids%weights * abs (process%grids%grids%f_max)
    norm = sum (weight)
    if (norm /= 0) then
       efficiency = &
            dot_product (process_get_vamp_efficiency_array (process), weight) &
            / norm
    else
       efficiency = 1
    end if
  end function process_get_vamp_efficiency

  subroutine process_integrate (process, rng, &
       grid_parameters, pass, it1, it2, calls, &
       discard_integrals, adapt_grids, adapt_weights, print_current, &
       time_estimate, &
       grids_filename, md5sum)
    type(process_t), intent(inout), target :: process
    type(tao_random_state), intent(inout) :: rng
    type(grid_parameters_t), intent(in) :: grid_parameters
    integer, intent(in) :: pass, it1, it2, calls
    logical, intent(in) :: discard_integrals
    logical, intent(in) :: adapt_grids
    logical, intent(in) :: adapt_weights
    logical, intent(in) :: print_current
    logical, intent(in) :: time_estimate
    type(string_t), intent(in), optional :: grids_filename
    type(md5sum_grids_t), intent(in), optional :: md5sum
    integer :: it
    real(default) :: integral, error, efficiency
    type(time_t) :: time_start, time_end
    type(md5sum_grids_t) :: md5sum_local
    real(default) :: sqrts
    real(default), dimension(:), allocatable :: grove_weight
    integer :: u
    if (it1 > it2)  return
    u = logfile_unit ()
    if (present (md5sum)) then
       md5sum_local = md5sum
       md5sum_local%process = process%md5sum
       md5sum_local%model = model_get_md5sum (process%model)
       md5sum_local%parameters = model_get_parameters_md5sum (process%model)
       md5sum_local%phs = process%md5sum_phs
       md5sum_local%alpha_s = process%md5sum_alpha_s
    end if
    sqrts = process%sqrts
    if (discard_integrals .and. it1==1) then
       if (grid_parameters%use_vamp_equivalences) then
          call vamp_discard_integrals (process%grids, &
               calls, stratified=grid_parameters%stratified, eq=process%vamp_eq)
       else
          call vamp_discard_integrals (process%grids, &
               calls, stratified=grid_parameters%stratified)
       end if
    end if
    process%beams_are_set = .false.
    do it = it1, it2
       if (adapt_grids) then
          call process_adapt_grids (process)
       end if
       if (adapt_weights) then
          call process_adapt_channel_weights (process, grid_parameters, calls)
       end if
       call process_status_reset_counters (process%status)
       if (time_estimate)  time_start = time_current ()
       if (grid_parameters%use_vamp_equivalences) then
          call vamp_sample_grids &
               (rng, process%grids, sample_function, process%store_index, 1, &
                eq=process%vamp_eq, &
                history=process%v_history(it:), &
                histories=process%v_histories(it:,:), &
                integral=integral, std_dev=error, negative_weights=&
                process%negative_weights)
       else
          call vamp_sample_grids &
               (rng, process%grids, sample_function, process%store_index, 1, &
                history=process%v_history(it:), &
                histories=process%v_histories(it:,:), &
                integral=integral, std_dev=error, negative_weights=&
                process%negative_weights)
       end if
       if (time_estimate)  time_end = time_current ()
       efficiency = process_get_vamp_efficiency (process)
       call process_get_grove_weights (process, grove_weight)
       if (time_estimate) then
          call integration_results_append (process%results, &
               process%type, pass, 1, calls, &
               integral, error, efficiency, grove_weight, time_start, time_end)
       else
          call integration_results_append (process%results, &
               process%type, pass, 1, calls, &
               integral, error, efficiency, grove_weight)
       end if
       if (present (grids_filename)) then
          call write_grid_file (grids_filename, process%id, md5sum_local, &
               grid_parameters, process%results, process%grids)
       end if
       if (print_current) then
          call integration_results_write_current (process%results)
          call integration_results_write_current (process%results, unit=u)
          if (u >= 0) flush (u)
       end if
       call integration_results_write_driver (process%results, process%id)
    end do
  end subroutine process_integrate

  subroutine process_do_dummy_integration (process)
    type(process_t), intent(inout) :: process
    call integration_results_append (process%results, &
         process%type, 1, 1, 0, &
         0._default, 0._default, 0._default)
  end subroutine process_do_dummy_integration

  subroutine process_me_test &
       (process, rng, n_calls, time_in_seconds, sample_function_sum)
    type(process_t), intent(inout), target :: process
    type(tao_random_state), intent(inout) :: rng
    integer, intent(in) :: n_calls
    real(default), intent(out), optional :: time_in_seconds, sample_function_sum
    integer :: prc_index, i
    type(time_t) :: time_start, time_end
    real(default), dimension(:), allocatable :: weights
    real(default) :: s
    process%beams_are_set = .false.
    s = 0
    allocate (weights (process%n_channels))
    weights = 1._default / size (weights)
    call process_status_reset_counters (process%status)
    if (present (time_in_seconds))  time_start = time_current ()
    do i = 1, n_calls
       s = s + sample_function &
                 (random_xi (), process%store_index, &
                  weights=weights, &
                  channel=random_channel ())
    end do
    if (present (time_in_seconds)) then
       time_end = time_current ()
       time_in_seconds = time_end - time_start
    end if
    if (present (sample_function_sum)) then
       sample_function_sum = s
    end if
  contains
    function random_channel () result (channel)
      integer :: channel
      real(default) :: x
      call tao_random_number (rng, x)
      channel = ceiling (x * process%n_channels)
    end function random_channel
    function random_xi () result (xi)
      real(default), dimension (process%n_par) :: xi
      integer :: i
      do i = 1, size (xi)
         call tao_random_number (rng, xi(i))
      end do
    end function random_xi
  end subroutine process_me_test

  subroutine process_init_vamp_history (process, n_iterations)
    type(process_t), intent(inout) :: process
    integer, intent(in) :: n_iterations
    call process_final_vamp_history (process)
    allocate (process%v_history (n_iterations))
    allocate (process%v_histories &
         (n_iterations, process_get_n_channels (process)))
    call vamp_create_history (process%v_history, verbose=.false.)
    call vamp_create_history (process%v_histories, verbose=.false.)
  end subroutine process_init_vamp_history

  subroutine process_final_vamp_history (process)
    type(process_t), intent(inout) :: process
    if (allocated (process%v_history)) then
       call vamp_delete_history (process%v_history)
       deallocate (process%v_history)
    end if
    if (allocated (process%v_histories)) then
       call vamp_delete_history (process%v_histories)
       deallocate (process%v_histories)
    end if
  end subroutine process_final_vamp_history

  subroutine process_write_time_estimate (process, unit)
    type(process_t), intent(in) :: process
    integer, intent(in), optional :: unit
    real(default) :: time_per_event, time_per_10k
    time_per_event = integration_results_get_time_per_event (process%results)
    time_per_10k = 10000 * time_per_event
    write (msg_buffer, "(A)")  "Process '" // char (process%id) // "': " 
    call msg_message ()
    write (msg_buffer, "(A)")  "   time estimate for generating " &
         // "10000 unweighted events: " &
         // char (time2string (int (time_per_10k)))
    call msg_message (unit=unit)
    call write_hline (unit)
  end subroutine process_write_time_estimate

  subroutine write_grid_file (filename, process_id, md5sum, &
       grid_parameters, results, grids)
    type(string_t), intent(in) :: filename, process_id
    type(md5sum_grids_t), intent(in) :: md5sum
    type(grid_parameters_t), intent(in) :: grid_parameters
    type(integration_results_t), intent(in) :: results
    type(vamp_grids), intent(in) :: grids
    integer :: u
    u = free_unit ()
    open (file = char (filename), unit = u, &
         action = "write", status = "replace")
    write (u, *) "process ", char (process_id)
    write (u, *) "  md5sum_process     = ", '"', md5sum%process, '"'
    write (u, *) "  md5sum_model       = ", '"', md5sum%model, '"'
    write (u, *) "  md5sum_parameters  = ", '"', md5sum%parameters, '"'
    write (u, *) "  md5sum_phase_space = ", '"', md5sum%phs, '"'
    write (u, *) "  md5sum_beams       = ", '"', md5sum%beams, '"'
    write (u, *) "  md5sum_sf_list     = ", '"', md5sum%sf_list, '"'
    write (u, *) "  md5sum_mappings    = ", '"', md5sum%mappings, '"'
    write (u, *) "  md5sum_cuts        = ", '"', md5sum%cuts, '"'
    write (u, *) "  md5sum_weight      = ", '"', md5sum%weight, '"'
    write (u, *) "  md5sum_scale       = ", '"', md5sum%scale, '"'
    write (u, *) "  md5sum_fac_scale   = ", '"', md5sum%fac_scale, '"'
    write (u, *) "  md5sum_ren_scale   = ", '"', md5sum%ren_scale, '"'    
    write (u, *) "  md5sum_alpha_s     = ", '"', md5sum%alpha_s, '"'    
    write (u, *)
    call grid_parameters_write (grid_parameters, u)
    write (u, *)
    call integration_results_write &
         (results, u, verbose = .true.)
    write (u, *)
    call vamp_write_grids (grids, u, write_integrals = .true.)
    close (u)
  end subroutine write_grid_file

  subroutine read_grid_file (filename, process_id, md5sum, &
       grid_parameters, results, grids, &
       pass, n_calls, ok)
    type(string_t), intent(in) :: filename, process_id
    type(md5sum_grids_t), intent(in) :: md5sum
    type(grid_parameters_t), intent(in) :: grid_parameters
    type(integration_results_t), intent(out) :: results
    type(vamp_grids), intent(inout) :: grids
    integer, dimension(:), intent(in) :: pass, n_calls
    logical, intent(out) :: ok
    integer :: u
    logical :: exist
    character(80) :: buffer
    character :: equals
    character(32) :: md5sum_file
    type(grid_parameters_t) :: grid_parameters_file
    type(integration_results_t) :: results_file
    ok = .false.
    inquire (file = char (filename), exist = exist)
    if (.not. exist)  return
    call msg_message ("Reading integration grids and results from file '" &
         // char (filename) // "':")
    u = free_unit ()
    open (file = char (filename), unit = u, action = "read", status = "old")
    read (u, *)  buffer
    if (trim (adjustl (buffer)) /= "process") then
       call msg_fatal ("Grid file: missing 'process' tag")
       close (u);  return
    end if
    read (u, *)  buffer, equals, md5sum_file
    if (md5sum_file /= md5sum%process) then
       call msg_message &
            ("Process configuration has changed, discarding old grid file")
       close (u);  return
    end if
    read (u, *)  buffer, equals, md5sum_file
    if (md5sum_file /= md5sum%model) then
       call msg_message &
            ("Model has changed, discarding old grid file")
       close (u);  return
    end if
    read (u, *)  buffer, equals, md5sum_file
    if (md5sum_file /= md5sum%parameters) then
       call msg_message &
            ("Model parameters have changed, discarding old grid file")
       close (u);  return
    end if
    read (u, *)  buffer, equals, md5sum_file
    if (md5sum_file /= md5sum%phs) then
       call msg_message &
            ("Phase-space setup has changed, discarding old grid file")
       close (u);  return
    end if
    read (u, *)  buffer, equals, md5sum_file
    if (md5sum_file /= md5sum%beams) then
       call msg_message &
            ("Beam setup has changed, discarding old grid file")
       close (u);  return
    end if
    read (u, *)  buffer, equals, md5sum_file
    if (md5sum_file /= md5sum%sf_list) then
       call msg_message &
            ("Structure-function setup has changed, discarding old grid file")
       close (u);  return
    end if
    read (u, *)  buffer, equals, md5sum_file
    if (md5sum_file /= md5sum%mappings) then
       call msg_message &
            ("Mapping scale parameters have changed, discarding old grid file")
       close (u);  return
    end if
    read (u, *)  buffer, equals, md5sum_file
    if (md5sum_file /= md5sum%cuts) then
       call msg_message &
            ("Cut configuration has changed, discarding old grid file")
       close (u);  return
    end if
    read (u, *)  buffer, equals, md5sum_file
    if (md5sum_file /= md5sum%weight) then
       call msg_message &
            ("Weight expression has changed, discarding old grid file")
       close (u);  return
    end if
    read (u, *)  buffer, equals, md5sum_file
    if (md5sum_file /= md5sum%scale) then
       call msg_message &
            ("General scale expression has changed, discarding old grid file")
       close (u);  return
    end if    
    read (u, *)  buffer, equals, md5sum_file
    if (md5sum_file /= md5sum%fac_scale) then
       call msg_message &
            ("Factorization scale expression has changed, discarding old grid file")
       close (u);  return
    end if
    read (u, *)  buffer, equals, md5sum_file
    if (md5sum_file /= md5sum%ren_scale) then
       call msg_message &
            ("Renormalization scale expression has changed, discarding old grid file")
       close (u);  return
    end if
    read (u, *)  buffer, equals, md5sum_file
    if (md5sum_file /= md5sum%alpha_s) then
       call msg_message &
            ("Alpha(QCD) specifications have changed, discarding old grid file")
       close (u);  return
    end if
    read (u, *)
    call grid_parameters_read (grid_parameters_file, u)
    if (grid_parameters_file /= grid_parameters) then
       call msg_message &
            ("Grid parameters have changed, discarding old grid file")
       close (u);  return
    end if
    read (u, *)
    call integration_results_read (results_file, u)
    if (.not. integration_results_iterations_are_consistent &
         (results_file, pass, n_calls)) then
       call msg_message &
            ("Iteration parameters have changed, discarding old grid file")
       close (u);  return
    end if
    results = results_file
    read (u, *)
    call vamp_read_grids (grids, u)
    close (u)
    ok = .true.
  end subroutine read_grid_file

  subroutine process_read_grid_file (process, filename, md5sum, &
       grid_parameters, pass, n_calls, ok)
    type(process_t), intent(inout) :: process
    type(string_t), intent(in) :: filename
    type(md5sum_grids_t), intent(in) :: md5sum
    type(grid_parameters_t), intent(in) :: grid_parameters
    integer, dimension(:), intent(in) :: pass, n_calls
    logical, intent(out) :: ok
    type(md5sum_grids_t) :: md5sum_local
    md5sum_local = md5sum
    md5sum_local%process = process%md5sum
    md5sum_local%model = model_get_md5sum (process%model)
    md5sum_local%parameters = model_get_parameters_md5sum (process%model)
    md5sum_local%phs = process%md5sum_phs
    md5sum_local%alpha_s = process%md5sum_alpha_s
    call read_grid_file (filename, process%id, md5sum_local, &
         grid_parameters, process%results, process%grids, &
         pass, n_calls, ok)
  end subroutine process_read_grid_file

  subroutine process_adapt_grids (process)
    type(process_t), intent(inout), target :: process
    call vamp_refine_grids (process%grids)
  end subroutine process_adapt_grids

  subroutine process_adapt_channel_weights (process, grid_parameters, calls)
    type(process_t), intent(inout), target :: process
    type(grid_parameters_t), intent(in) :: grid_parameters
    integer, intent(in) :: calls
    real(default), dimension(:), allocatable :: weights
    integer :: g, i0, i1, n
    real(default) :: sum_weights, weight_min
    logical, dimension(:), allocatable :: weight_underflow
    real(default) :: sum_weight_underflow
    integer :: n_underflow
    allocate (weights (process%n_channels))
    weights = process%grids%weights &
         * vamp_get_variance (process%grids%grids) &
           ** grid_parameters%channel_weights_power
    do g = 1, phs_forest_get_n_groves (process%forest)
       call phs_forest_get_grove_bounds (process%forest, g, i0, i1, n)
       weights(i0:i1) = sum (weights(i0:i1)) / n
    end do
    sum_weights = sum (weights)
    if (sum_weights /= 0) then
       weights = weights / sum (weights)
       if (grid_parameters%threshold_calls /= 0) then
          weight_min = &
               real (grid_parameters%threshold_calls, default) &
               / calls
          allocate (weight_underflow (process%n_channels))
          weight_underflow = weights /= 0 .and. weights < weight_min
          n_underflow = count (weight_underflow)
          sum_weight_underflow = sum (weights, mask=weight_underflow)
          where (weight_underflow)
             weights = weight_min
          elsewhere
             weights = weights &
                  * (1 - n_underflow * weight_min) / (1 - sum_weight_underflow)
          end where
       end if
       call vamp_update_weights (process%grids, weights)
    end if
  end subroutine process_adapt_channel_weights

  subroutine process_get_grove_weights (process, grove_weight)
    type(process_t), intent(in) :: process
    real(default), dimension(:), allocatable, intent(out) :: grove_weight
    integer :: n_groves, g, i0, i1, n
    n_groves = phs_forest_get_n_groves (process%forest) 
    allocate (grove_weight (n_groves))
    do g = 1, n_groves
       call phs_forest_get_grove_bounds (process%forest, g, i0, i1, n)
       grove_weight(g) = sum (process%grids%weights(i0:i1))
    end do
  end subroutine process_get_grove_weights

  subroutine process_setup_event_generation (process, qn_mask_in)
    type(process_t), intent(inout), target :: process
    type(quantum_numbers_mask_t), intent(in), optional :: qn_mask_in
    integer, dimension(:), allocatable :: coll_index
    type(quantum_numbers_mask_t), dimension(:), allocatable :: mask_in
    type(quantum_numbers_mask_t) :: mask_conn_sqme, mask_conn_flows
    type(evaluator_t), pointer :: eval_sfchain, eval_sqme, eval_flows
    type(interaction_t), pointer :: int_hi, int_beam
    integer :: n_in, n_out, n_tot, i
    type(evaluator_t), target :: eval_con
    if (.not. process_has_matrix_element (process)) then
       call msg_warning ("Process '" // char (process%id) // "': " &
            // "matrix element vanishes, no events can be generated")
       return
    end if
    call process_status_reset_counters (process%status)
    call hard_interaction_final_sqme (process%hi)
    call hard_interaction_final_flows (process%hi)
    call evaluator_final (process%eval_beam_flows)
    call evaluator_final (process%eval_sqme)
    call evaluator_final (process%eval_flows)
    n_in  = hard_interaction_get_n_in  (process%hi)
    n_out = hard_interaction_get_n_out (process%hi)
    n_tot = hard_interaction_get_n_tot (process%hi)
    int_hi => hard_interaction_get_int_ptr (process%hi)
    call interaction_reset_momenta (int_hi)
    allocate (mask_in (n_in))
    if (process%use_beams) then
       allocate (coll_index (n_in))
       coll_index = strfun_chain_get_colliding_particles (process%sfchain)
       mask_in = strfun_chain_get_colliding_particles_mask (process%sfchain)
       mask_conn_sqme = new_quantum_numbers_mask (.false., .true., .true.)
       mask_conn_flows = new_quantum_numbers_mask (.false., .false., .true.)
    else if (present (qn_mask_in)) then
       mask_in = qn_mask_in
    else
       mask_in = new_quantum_numbers_mask (.false., .false., .true.)
    end if
    call hard_interaction_init_sqme  (process%hi, mask_in, &
         process%use_hi_color_factors)
    call hard_interaction_init_flows (process%hi, mask_in)
    int_beam     => strfun_chain_get_beam_int_ptr       (process%sfchain)
    eval_sfchain => strfun_chain_get_last_evaluator_ptr (process%sfchain)
    eval_sqme    => hard_interaction_get_eval_sqme_ptr  (process%hi)
    eval_flows   => hard_interaction_get_eval_flows_ptr (process%hi)
!     print *, "Hard interaction"
!     call interaction_write (int_hi)
!     print *, "Evaluator: HI: SQME"
!     call evaluator_write (eval_sqme)
!     print *, "Evaluator: HI: flows"
!     call evaluator_write (eval_flows)
    if (process%use_beams) then
       if (associated (eval_sfchain)) then
          call evaluator_init_color_contractions &
               (process%eval_beam_flows, evaluator_get_int_ptr (eval_sfchain))
          do i = 1, n_in
             call evaluator_set_source_link &
                  (eval_sqme,  i, eval_sfchain, coll_index(i))
             call evaluator_set_source_link &
                  (eval_flows, i, process%eval_beam_flows, coll_index(i))
          end do
          if (process%has_extra_evaluators) then
             call evaluator_init_product (process%eval_sqme, &
                  eval_sfchain, eval_sqme,  mask_conn_sqme)
             call evaluator_init_product (process%eval_flows, &
                  process%eval_beam_flows, eval_flows, mask_conn_flows)
          end if
       else
          call evaluator_init_color_contractions &
               (process%eval_beam_flows, int_beam)
          do i = 1, n_in
             call evaluator_set_source_link &
                  (eval_sqme,  i, int_beam, coll_index(i))
             call evaluator_set_source_link &
                  (eval_flows, i, process%eval_beam_flows, coll_index(i))
          end do
          if (process%has_extra_evaluators) then
             call evaluator_init_product (process%eval_sqme, &
                  int_beam, eval_sqme, mask_conn_sqme)
             call evaluator_init_product (process%eval_flows, &
                  process%eval_beam_flows, eval_flows, mask_conn_flows)
          end if
       end if
!        print *, "Evaluator: Beams+HI: SQME"
!        call evaluator_write (process%eval_sqme)
!        print *, "Evaluator: Beams+HI: flows"
!        call evaluator_write (process%eval_flows)
!        call process_write (process, 77)
    end if
  end subroutine process_setup_event_generation

  subroutine process_generate_weighted_event (process, rng, weight)
    type(process_t), intent(inout), target :: process
    type(tao_random_state), intent(inout) :: rng
    real(default), intent(out) :: weight
    real(default), dimension(process%n_par) :: x
    call vamp_next_event &
         (x, rng, process%grids, &
          sample_function, process%store_index, phi_trivial, &
          weight=weight)
    call process_complete_evaluators (process)
  end subroutine process_generate_weighted_event
    
  subroutine process_generate_unweighted_event (process, rng, excess)
    type(process_t), intent(inout), target :: process
    type(tao_random_state), intent(inout) :: rng
    real(default), intent(out), optional :: excess
    real(default), dimension(process%n_par) :: x
    call vamp_next_event &
         (x, rng, process%grids, &
          sample_function, process%store_index, phi_trivial, &
          excess=excess)
    call process_complete_evaluators (process)
  end subroutine process_generate_unweighted_event

  function phi_trivial (xi, channel_dummy) result (x)
    real(default), dimension(:), intent(in) :: xi
    integer, intent(in) :: channel_dummy
    real(default), dimension(size(xi)) :: x
    x = xi
  end function phi_trivial

  subroutine process_complete_evaluators (process)
    type(process_t), intent(inout), target :: process
    if (process%use_beams) then
       call evaluator_receive_momenta (process%eval_beam_flows)
       call evaluator_evaluate (process%eval_beam_flows)
    end if
    call hard_interaction_evaluate_sqme (process%hi)
    call hard_interaction_evaluate_flows (process%hi)
    if (process%has_extra_evaluators) then
       call evaluator_receive_momenta (process%eval_sqme)
       call evaluator_receive_momenta (process%eval_flows)
       call evaluator_evaluate (process%eval_sqme)
       call evaluator_evaluate (process%eval_flows)
    end if
  end subroutine process_complete_evaluators

  subroutine process_get_unstable_products (process, flv_unstable)
    type(process_t), intent(in) :: process
    type(flavor_t), dimension(:), intent(out), allocatable :: flv_unstable
    call hard_interaction_get_unstable_products (process%hi, flv_unstable)
  end subroutine process_get_unstable_products

  subroutine process_set_particles (process, particle_set)
    type(process_t), intent(inout) :: process
    type(particle_set_t), intent(in) :: particle_set
    call particle_set_to_subevt (particle_set, process%subevt)
  end subroutine process_set_particles

  subroutine process_results_write_header (process, unit, logfile)
    type(process_t), intent(in) :: process
    integer, intent(in), optional :: unit
    logical, intent(in), optional :: logfile
    call write_dline (unit)
    call write_header (process%type, unit, logfile)
    call write_dline (unit)
  end subroutine process_results_write_header

  subroutine process_results_write_entry (process, it, unit)
    type(process_t), intent(in) :: process
    integer, intent(in) :: it
    integer, intent(in), optional :: unit
    call integration_results_write_entry (process%results, it, unit)
  end subroutine process_results_write_entry

  subroutine process_results_write_current (process, unit)
    type(process_t), intent(in) :: process
    integer, intent(in), optional :: unit
    call integration_results_write_current (process%results, unit)
  end subroutine process_results_write_current

  subroutine process_results_write_average (process, pass, unit)
    type(process_t), intent(in) :: process
    integer, intent(in) :: pass
    integer, intent(in), optional :: unit
    call write_hline (unit)
    call integration_results_write_average (process%results, pass, unit)
    call write_hline (unit)
  end subroutine process_results_write_average

  subroutine process_results_write_current_average (process, unit)
    type(process_t), intent(in) :: process
    integer, intent(in), optional :: unit
    call write_hline (unit)
    call integration_results_write_current_average (process%results, unit)
    call write_hline (unit)
  end subroutine process_results_write_current_average

  subroutine process_results_write_footer (process, unit, no_line)
    type(process_t), intent(in) :: process
    integer, intent(in), optional :: unit
    logical, intent(in), optional :: no_line
    if (present (no_line)) then
       if (.not. no_line)  call write_dline (unit)
    else
       call write_dline (unit)
    end if
    call integration_results_write_current_average (process%results, unit)
    call write_dline (unit)
  end subroutine process_results_write_footer

  subroutine process_results_write (process, unit)
    type(process_t), intent(in) :: process
    integer, intent(in), optional :: unit
    call integration_results_write (process%results, unit)
  end subroutine process_results_write

  subroutine process_record_integral (process, var_list)
    type(process_t), intent(inout) :: process
    type(var_list_t), intent(inout) :: var_list
    integer :: n_calls
    real(default) :: integral, error, accuracy, chi2, efficiency
    n_calls = integration_results_get_n_calls (process%results)
    integral = integration_results_get_integral (process%results)
    error = integration_results_get_error (process%results)
    accuracy = integration_results_get_accuracy (process%results)
    chi2 = integration_results_get_chi2 (process%results)
    efficiency = integration_results_get_efficiency (process%results)
    call var_list_init_process_results (var_list, process%id, &
         n_calls, integral, error, accuracy, chi2, efficiency)
  end subroutine process_record_integral
    
  subroutine process_make_copy (process, original)
    type(process_t), intent(inout), target :: process
    type(process_t), intent(in), target :: original
    type(process_t), pointer :: copy
    type(interaction_t), pointer :: beam_int, copy_beam_int
    type(interaction_t), pointer :: hi_int, copy_hi_int
    type(evaluator_t), pointer :: hi_eval_trace, copy_hi_eval_trace
    type(evaluator_t), pointer :: hi_eval_sqme, copy_hi_eval_sqme
    type(evaluator_t), pointer :: hi_eval_flows, copy_hi_eval_flows
    allocate (copy)
    copy%type = original%type
    copy%is_original = .false.
    copy%original => original
    copy%initialized = original%initialized
    copy%has_matrix_element = original%has_matrix_element
    copy%use_hi_color_factors = original%use_hi_color_factors
    copy%use_beams = original%use_beams
    copy%has_extra_evaluators = .false.
    copy%beams_are_set = original%beams_are_set
    copy%is_cascade_decay = original%is_cascade_decay
    copy%id = original%id
    copy%prc_lib => original%prc_lib
    copy%lib_index = original%lib_index
    copy%store_index = original%store_index
    copy%model => original%model
    if (original%use_beams) then
       copy%n_strfun = original%n_strfun
       copy%n_par_strfun = original%n_par_strfun
    end if
    copy%n_par_hi = original%n_par_hi
    copy%n_par = original%n_par
    copy%azimuthal_dependence = original%azimuthal_dependence
    copy%vamp_grids_defined = original%vamp_grids_defined
    copy%sqrts_known = original%sqrts_known
    copy%sqrts = original%sqrts
    if (original%use_beams) then
       if (allocated (original%x_strfun)) &
            allocate (copy%x_strfun (size (original%x_strfun)))
    end if
    if (allocated (original%x_hi)) &
         allocate (copy%x_hi (size (original%x_hi)))
    copy%n_channels = original%n_channels
    if (allocated (original%x)) &
         allocate (copy%x (size (original%x, 1), size (original%x, 2)))
    if (allocated (original%phs_factor)) &
         allocate (copy%phs_factor (size (original%phs_factor)))
    if (allocated (original%mass_in)) then
       allocate (copy%mass_in (size (original%mass_in)))
       copy%mass_in = original%mass_in
    end if
    copy%averaging_factor = original%averaging_factor
    if (original%use_beams)  copy%sfchain = original%sfchain
    copy%hi = original%hi
    if (original%use_beams)  copy%eval_trace = original%eval_trace
    !  copy%eval_beam_flows = original%eval_beam_flows
    !  copy%eval_sqme = original%eval_sqme
    !  copy%eval_flows = original%eval_flows
    copy%forest = original%forest
    copy%vamp_eq = original%vamp_eq
    allocate (copy%j_beam (size (original%j_beam)))
    copy%j_beam = original%j_beam
    allocate (copy%j_in (size (original%j_in)))
    copy%j_in = original%j_in
    allocate (copy%j_out (size (original%j_out)))
    copy%j_out = original%j_out
    copy%subevt = original%subevt
    copy%var_list = original%var_list
    copy%cut_pn => original%cut_pn
    copy%weight_pn => original%weight_pn
    copy%scale_pn => original%scale_pn    
    copy%fac_scale_pn => original%fac_scale_pn
    copy%ren_scale_pn => original%ren_scale_pn    
    copy%cut_expr = original%cut_expr
    copy%reweighting_expr = original%reweighting_expr
    copy%scale_expr = original%scale_expr    
    copy%fac_scale_expr = original%fac_scale_expr
    copy%ren_scale_expr = original%ren_scale_expr    
    if (allocated (original%active_channel)) then
       allocate (copy%active_channel (size (original%active_channel)))
       copy%active_channel = original%active_channel
    end if
    call vamp_copy_grids (copy%grids, original%grids)
    beam_int => strfun_chain_get_beam_int_ptr (original%sfchain)
    hi_int => hard_interaction_get_int_ptr (original%hi)
    hi_eval_trace => hard_interaction_get_eval_trace_ptr (original%hi)
    hi_eval_sqme => hard_interaction_get_eval_sqme_ptr (original%hi)
    hi_eval_flows => hard_interaction_get_eval_flows_ptr (original%hi)
    copy_beam_int => strfun_chain_get_beam_int_ptr (copy%sfchain)
    copy_hi_int => hard_interaction_get_int_ptr (copy%hi)
    copy_hi_eval_trace => hard_interaction_get_eval_trace_ptr (copy%hi)
    copy_hi_eval_sqme => hard_interaction_get_eval_sqme_ptr (copy%hi)
    copy_hi_eval_flows => hard_interaction_get_eval_flows_ptr (copy%hi)
    select case (original%type)
    case (PRC_SCATTERING)
       call msg_bug ("Process copy for scattering processes not implemented")
    case (PRC_DECAY)
       call interaction_reassign_links &
            (copy_hi_int, beam_int, copy_beam_int)
       call evaluator_reassign_links &
            (copy_hi_eval_trace, beam_int, copy_beam_int)
       call evaluator_reassign_links &
            (copy_hi_eval_sqme, beam_int, copy_beam_int)
       call evaluator_reassign_links &
            (copy_hi_eval_flows, beam_int, copy_beam_int)
       call evaluator_reassign_links &
            (copy_hi_eval_trace, hi_int, copy_hi_int)
       call evaluator_reassign_links &
            (copy_hi_eval_sqme, hi_int, copy_hi_int)
       call evaluator_reassign_links &
            (copy_hi_eval_flows, hi_int, copy_hi_int)
       call evaluator_reassign_links &
            (copy%eval_trace, beam_int, copy_beam_int)
       call evaluator_reassign_links &
            (copy%eval_sqme, beam_int, copy_beam_int)
       call evaluator_reassign_links &
            (copy%eval_flows, beam_int, copy_beam_int)
       call evaluator_reassign_links &
            (copy%eval_trace, hi_eval_trace, copy_hi_eval_trace)
       call evaluator_reassign_links &
            (copy%eval_sqme, hi_eval_sqme, copy_hi_eval_sqme)
       call evaluator_reassign_links &
            (copy%eval_flows, hi_eval_flows, copy_hi_eval_flows)
    end select
    process%copy => copy
  end subroutine process_make_copy

  recursive subroutine process_request_copy (process, copy, original)
    type(process_t), intent(inout), target :: process
    type(process_t), pointer :: copy
    type(process_t), intent(inout), target, optional :: original
    if (associated (process%copy)) then
       if (process%copy%in_use) then
          if (present (original)) then
             call process_request_copy (process%copy, copy, original)
          else
             call process_request_copy (process%copy, copy, process)
          end if
       else
          copy => process%copy
          copy%in_use = .true.
          if (present (original)) then
             original%working_copy => copy
          else
             process%working_copy => copy
          end if
       end if
    else
       if (present (original)) then
          call process_make_copy (process, original)
       else
          call process_make_copy (process, original=process)
       end if
       copy => process%copy
       copy%in_use = .true.
       if (present (original)) then
          original%working_copy => copy
       else
          process%working_copy => copy
       end if
    end if
  end subroutine process_request_copy

  function process_get_working_copy_ptr (process) result (copy)
    type(process_t), intent(in), target :: process 
    type(process_t), pointer :: copy
    if (associated (process%working_copy)) then
       copy => process%working_copy
    else
       copy => process
    end if
  end function process_get_working_copy_ptr

  subroutine process_tag_as_working_copy (process)
    type(process_t), intent(inout), target :: process 
    type(process_t), pointer :: original
    if (associated (process%original)) then
       original => process%original
       original%working_copy => process
    else
       call msg_bug ("Process tag as working copy failed")
    end if
  end subroutine process_tag_as_working_copy

  subroutine process_free_copy (process)
    type(process_t), intent(inout), target :: process
    process%in_use = .false.
    if (associated (process%original)) then
       process%original%working_copy => null ()
    end if
  end subroutine process_free_copy

  recursive subroutine process_delete_copies (process)
    type(process_t), intent(inout), target :: process
    if (associated (process%copy)) then
       call process_final (process%copy)
       deallocate (process%copy)
    end if
  end subroutine process_delete_copies

  subroutine process_store_final ()
    type(process_entry_t), pointer :: current
    if (allocated (store%proc))  deallocate (store%proc)
    store%last => null ()
    do while (associated (store%first))
       current => store%first
       store%first => current%next
       call process_final (current%process)
       deallocate (current)
    end do
    store%n = 0
  end subroutine process_store_final

  subroutine process_store_unload (libname)
    type(string_t), intent(in) :: libname
    type(process_entry_t), pointer :: entry
    entry => store%first
    do while (associated (entry))
       if (process_library_get_name (entry%process%prc_lib) == libname) &
          call worker (entry%process)
       entry => entry%next
    end do
  
  contains

    recursive subroutine worker (process)
      type(process_t), intent(inout), target :: process
      call hard_interaction_unload (process%hi)
      if (associated (process%copy)) call worker (process%copy)
    end subroutine worker

  end subroutine process_store_unload

  subroutine process_store_reload (libname)
    type(string_t), intent(in) :: libname
    type(process_entry_t), pointer :: entry
    entry => store%first
    do while (associated (entry))
       if (process_library_get_name (entry%process%prc_lib) == libname) &
          call worker (entry%process)
       entry => entry%next
    end do

  contains
  
    recursive subroutine worker (process)
      type(process_t), intent(inout), target :: process
      call hard_interaction_reload (process%hi, process%prc_lib)
      if (associated (process%copy)) call worker (process%copy)
    end subroutine worker

  end subroutine process_store_reload

  subroutine process_store_write (unit)
    integer, intent(in), optional :: unit
    type(process_t), pointer :: process
    integer :: u, i
    u = output_unit (unit);  if (u < 0)  return
    write (u, *)  repeat ("%", 78)
    write (u, *)  "Process store contents"
    do i = 1, store%n
       write (u, *)  repeat ("%", 78)
       write (u, *)  "Process No.", i
       process => store%proc(i)%ptr
       call process_write (process, unit)
    end do
    write (u, *)  "Process store end"
    write (u, *)  repeat ("%", 78)
  end subroutine process_store_write

  subroutine process_store_write_results (unit)
    integer, intent(in), optional :: unit
    type(process_t), pointer :: process
    type(string_t), dimension(:), allocatable :: process_id
    real(default), dimension(:), allocatable :: integral, error
    type(string_t), dimension(:), allocatable :: phys_unit
    integer :: u, i, process_id_len
    character(12) :: fmt
    u = output_unit (unit);  if (u < 0)  return
    allocate (process_id (store%n), phys_unit (store%n))
    allocate (integral (store%n), error (store%n))
    do i = 1, store%n
       process => store%proc(i)%ptr
       if (process%initialized) then
          process_id(i) = process%id
          integral(i) = process_get_integral (process)
          error(i) = process_get_error (process)
          select case (process%type)
          case (PRC_DECAY);       phys_unit(i) = "GeV"
          case (PRC_SCATTERING);  phys_unit(i) = "fb"
          case default;           phys_unit(i) = "[undefined]"
          end select
       else
          process_id(i) = ""
       end if
    end do
    write (u, "(A)")  "|=========================      Results Summary      =========================|"
    if (store%n == 0) then
       write (u, *) "[empty]"
    else
       process_id_len = maxval (len (process_id))
       write (fmt, "(A,I0,A)")  "(1x,A", process_id_len + 1, ")"
       do i = 1, store%n
          if (process_id(i) /= "") then
             write (u, fmt, advance="no")  char (process_id(i)) // ":"
             write (u, "(1x, 1PE15.8, 1x, '+-', 1x, 1PE8.2)", advance="no") &
                  integral(i), error(i)
             write (u, "(1x, A)")  char (phys_unit(i))
          end if
       end do
    end if
    write (u, "(A)")  "|=============================================================================|"
  end subroutine process_store_write_results

  function process_store_get_n_processes () result (n)
    integer :: n
    n = store%n
  end function process_store_get_n_processes

  function process_store_get_entry_ptr (process_id) result (entry)
    type(process_entry_t), pointer :: entry
    type(string_t), intent(in) :: process_id
    entry => store%first
    do while (associated (entry))
       if (entry%process%id == process_id)  exit
       entry => entry%next
    end do
  end function process_store_get_entry_ptr

  function process_store_get_process_index (process_id) result (process_index)
    integer :: process_index
    type(string_t), intent(in) :: process_id
    type(process_entry_t), pointer :: entry
    entry => process_store_get_entry_ptr (process_id)
    if (associated (entry)) then
       process_index = entry%process%store_index
    else
       process_index = 0
    end if
  end function process_store_get_process_index

  function process_store_get_process_ptr_int (i) result (process)
    type(process_t), pointer :: process
    integer, intent(in) :: i
    if (i > 0 .and. i <= size (store%proc)) then
       process => store%proc(i)%ptr
    else
       process => null ()
    end if
  end function process_store_get_process_ptr_int

  function process_store_get_process_ptr_id (id) result (process)
    type(process_t), pointer :: process
    type(string_t), intent(in) :: id
    integer :: i
    do i = 1, store%n
       process => store%proc(i)%ptr
       if (process%id == id)  return
    end do
    process => null ()
  end function process_store_get_process_ptr_id

  function process_store_get_fresh_process_ptr (process_id) result (process)
    type(process_t), pointer :: process
    type(string_t), intent(in) :: process_id
    type(process_entry_t), pointer :: current, entry
    integer :: i
    integer, parameter :: BLOCK_SIZE = 10
    current => process_store_get_entry_ptr (process_id)
    if (associated (current)) then
       call process_final (current%process)
    else
       allocate (current)
       if (store%n == 0) then
          allocate (store%proc (BLOCK_SIZE))
          store%first => current
       else
          store%last%next => current
       end if
       store%last => current
       store%n = store%n + 1
       if (store%n <= size (store%proc)) then
          store%proc(store%n)%ptr => current%process
       else
          deallocate (store%proc)
          allocate (store%proc (store%n + BLOCK_SIZE))
          i = 1
          entry => store%first
          do while (associated (entry))
             store%proc(i)%ptr => entry%process;  i = i + 1
             entry => entry%next
          end do
       end if
    end if
    process => current%process
  end function process_store_get_fresh_process_ptr

  subroutine process_store_init_process (process, &
       prc_lib, process_id, model, lhapdf_status, var_list, &
       use_beams, allow_global_mapping)
    type(process_t), pointer :: process
    type(process_library_t), intent(inout), target :: prc_lib
    type(string_t), intent(in) :: process_id
    type(model_t), intent(in), target :: model
    type(lhapdf_status_t), intent(inout) :: lhapdf_status
    type(var_list_t), intent(in), target :: var_list
    logical, intent(in) :: use_beams
    logical, intent(in), optional :: allow_global_mapping
    integer :: process_lib_index, process_store_index
    procedure(prclib_unload_hook), pointer :: unload_hook
    procedure(prclib_reload_hook), pointer :: reload_hook
    process_lib_index = process_library_get_process_index (prc_lib, process_id)
    if (process_lib_index == 0) then
       call msg_fatal ("Process '" // char (process_id) &
            // "' is not available.")
    end if
    process_store_index = process_store_get_process_index (process_id)
    process => process_store_get_fresh_process_ptr (process_id)
    if (process_store_index == 0) then
       process_store_index = process_store_get_n_processes ()
    end if
    unload_hook => process_store_unload
    reload_hook => process_store_reload
    call process_library_set_unload_hook (prc_lib, unload_hook)
    call process_library_set_reload_hook (prc_lib, reload_hook)
    call process_init &
         (process, prc_lib, process_lib_index, process_store_index, &
          process_id, model, lhapdf_status, var_list, use_beams)
    if (present (allow_global_mapping)) then
       if (allow_global_mapping)  call process_allow_global_mapping (process)
    end if
  end subroutine process_store_init_process

  function sample_function (xi, prc_index, weights, channel, grids) result (f)
    real(default) :: f
    real(default), dimension(:), intent(in) :: xi
    integer, intent(in) :: prc_index
    real(default), dimension(:), intent(in), optional :: weights
    integer, intent(in), optional :: channel
    type(vamp_grid), dimension(:), intent(in), optional :: grids
    type(process_t), pointer :: process
    logical :: ok
    call terminate_now_if_signal ()
    process => process_get_working_copy_ptr (store%proc(prc_index)%ptr)
    call process_status_reset_flags (process%status)
    call process_status_called (process%status)
    call process_set_kinematics (process, xi, channel, ok)
    if (ok) then
       call process_fill_subevt (process, transform=process%is_cascade_decay)
       ok = process_passes_cuts (process)
       if (ok)  call process_status_passed_cuts (process%status)
    end if
    call terminate_now_if_signal ()
    if (ok) then
       call process_complete_kinematics (process, channel)
       if (present (grids)) then
          call process_compute_vamp_phs_factor (process, weights)
       else
          process%vamp_phs_factor = 1
       end if
       call process_compute_scale (process)
       call process_update_alpha_s (process)
       call process_evaluate (process)
       call process_compute_reweighting_factor (process)
       process%sample_function_value = &
            process%flux_factor &
            * process%sf_mapping_factor &
            * process%vamp_phs_factor &
            * process%phs_volume &
            * process%sqme &
            * process%reweighting_factor
       call process_status_passed_evaluation (process%status)
    else
       process%sample_function_value = 0
    end if
    f = process%sample_function_value
    call terminate_now_if_signal ()
  end function sample_function

  subroutine process_test ()
    type(os_data_t), pointer :: os_data => null ()
    type(process_library_t), pointer :: prc_lib => null ()
    type(model_t), pointer :: model => null ()
    type(var_list_t), pointer :: var_list => null ()
    allocate (os_data)
    allocate (prc_lib)
    allocate (var_list)
    call process_library_store_final
    call os_data_init (os_data)
    print *, "*** Read model file"
    call syntax_model_file_init ()
    call model_list_read_model &
         (var_str("SM"), var_str("SM.mdl"), os_data, model)
    var_list => model_get_var_list_ptr (model)
    call syntax_pexpr_init ()
    call syntax_phs_forest_init ()
    print *
    print *, "*** Create process library"
    call var_list_append_string (var_list, name = "$library_name", sval = "prc_proc")
    call var_list_append_log (var_list, name = "?read_color_factors", lval = .true.)
    call var_list_append_log (var_list, name = "?alpha_s_is_fixed", lval = .true.)
    call process_library_store_append (var_str ("prc_proc"), os_data, prc_lib)
    call process_library_init (prc_lib, var_str("prc_proc"), os_data)
    print *
    call process_test1 (prc_lib, os_data, model, var_list)
    print *
    call process_test2 (prc_lib, os_data, model, var_list)
    print *
    call process_test3 (prc_lib, os_data, model, var_list)
    print *
    call process_test4 (prc_lib, os_data, model, var_list)
    print *
    print *, "* Cleanup"
    call process_store_final ()
    call syntax_pexpr_final ()
    call syntax_phs_forest_final ()
    call syntax_model_file_final ()
    call process_library_final (prc_lib)
    deallocate (prc_lib)
    deallocate (os_data)
  end subroutine process_test

  subroutine process_test1 (prc_lib, os_data, model, var_list)
    type(process_library_t), intent(inout) :: prc_lib
    type(model_t), intent(in), target :: model
    type(var_list_t), intent(inout), target :: var_list
    type(process_t), pointer :: process
    type(string_t) :: objlist
    type(string_t), dimension(:), allocatable :: prt_in, prt_out
    type(lhapdf_status_t) :: lhapdf_status
    type(os_data_t), intent(inout) :: os_data
    type(phs_parameters_t) :: phs_par
    type(mapping_defaults_t) :: mapping_defaults
    type(flavor_t), dimension(1) :: flv
    type(polarization_t), dimension(1) :: pol
    type(beam_data_t) :: beam_data
    type(grid_parameters_t) :: grid_parameters
    type(tao_random_state) :: rng
    integer :: i
    logical :: rebuild_phs = .true.
    logical :: discard_integrals, adapt_grids, adapt_weights, print_current
    logical :: time_estimate = .true.
    print *, "*** Test decay process Z -> e+ e- ***"
    print *
    print *, "* Initialization"
    call tao_random_create (rng, 0)
    allocate (prt_in (1), prt_out (2))
    print *, "setting particles for Z -> e+ e-"
    prt_in(1) = "Z"
    prt_out(1) = "e1"
    prt_out(2) = "E1"
    call process_library_append &
         (prc_lib, var_str ("zff"), model, prt_in, prt_out, method = PRC_TEST, &
              message = .true. )
    deallocate (prt_in, prt_out)
    allocate (prt_in (1), prt_out (2))
    print *, "setting particles for Z -> u ubar"
    prt_in(1) = "Z"
    prt_out(1) = "u"
    prt_out(2) = "U"
    call process_library_append &
         (prc_lib, var_str ("zqq"), model, prt_in, prt_out, method = PRC_TEST, &
              message = .true. )
    deallocate (prt_in, prt_out)
    allocate (prt_in (2), prt_out (3))
    print *, "setting particles for e+ e- -> nu nubar H"
    prt_in(1) = "e1"
    prt_in(2) = "E1"
    prt_out(1) = "nue"
    prt_out(2) = "nuebar"
    prt_out(3) = "H"
    call process_library_append &
         (prc_lib, var_str ("nnh"), model, prt_in, prt_out, method = PRC_TEST, &
              message = .true. )
    deallocate (prt_in, prt_out)
    allocate (prt_in (2), prt_out (2))
    print *, "setting particles for g g -> u ubar"
    prt_in(1) = "g"
    prt_in(2) = "g"
    prt_out(1) = "u"
    prt_out(2) = "U"
    call process_library_append &
         (prc_lib, var_str ("gguu"), model, prt_in, prt_out, method = PRC_TEST, &
              message = .true. )
    deallocate (prt_in, prt_out)
    print *
    print *, "* Generate code"
    call process_library_generate_code (prc_lib, os_data)
    print *
    print *, "* Write driver file 'prc_proc_interface.f90'"
    call process_library_write_driver (prc_lib)
    print *
    print *, "* Compile and link as 'libprc_proc.so'"
    call process_library_compile (prc_lib, os_data, .false., objlist)
    call process_library_link (prc_lib, os_data, objlist)
    print *
    print *, "* Load shared libraries"
    call process_library_load (prc_lib, os_data, var_list = var_list)
    print *
    call process_store_init_process &
         (process, prc_lib, var_str ("zff"), model, lhapdf_status, &
         var_list, use_beams = .true.)
    print *
    print *, "*** Beam/strfun setup (unpolarized)"
    print *
    call flavor_init (flv, (/ 23 /), model)
    call polarization_init_unpolarized (pol(1), flv(1))
    call beam_data_init_decay (beam_data, flv, pol)
    call process_setup_beams (process, beam_data, 0, 0)
    call process_connect_strfun (process)
    call process_setup_subevt (process)
    print *
    print *, "* Phase space setup"
    call openmp_set_num_threads_verbose (1) 
    call process_setup_phase_space (process, rebuild_phs, &
         os_data, phs_par, mapping_defaults, filename_out=var_str("zff.phs"), &
         vis_channels = .false.)
    call process_init_vamp_history (process, 1)
    print *
    print *, "*** Test integration"
    print *, "* Grids setup"
    grid_parameters%stratified = .false.
    call process_setup_grids (process, grid_parameters, calls=9)
    print *
    print *, "* 1 iteration with minimal number of calls"
    call process_results_write_header (process)
    do i = 1, 1
       discard_integrals = i==1
       adapt_grids = .true.
       adapt_weights = .true.
       print_current = .true.
       call process_integrate (process, rng, grid_parameters, &
            1, 1, 1, 9, &
            discard_integrals, adapt_grids, adapt_weights, print_current, &
            time_estimate)
    end do
    call process_results_write_footer (process)
    print *
    print *, "* Process written to 'fort.60'"
    call process_write (process, 60)
    print *
    print *, "*** Beam/strfun setup (polarized)"
    call process_store_init_process &
         (process, prc_lib, var_str ("zff"), model, lhapdf_status, &
         var_list, use_beams = .true.)
    call flavor_init (flv, (/ 23 /), model)
    call polarization_init_axis &
         (pol(1), flv(1), (/ 0._default, 0._default, 1._default/))
    call beam_data_init_decay (beam_data, flv, pol)
    call process_setup_beams (process, beam_data, 0, 0)
    call process_connect_strfun (process)
    call process_setup_subevt (process)
    print *
    print *, "* Phase space setup"
    call openmp_set_num_threads_verbose (1) 
    call process_setup_phase_space (process, rebuild_phs, &
         os_data, phs_par, mapping_defaults, filename_out=var_str("zff.phs"), &
         vis_channels = .false.)
    call process_init_vamp_history (process, 6)
    print *
    print *, "*** Test integration"
    print *, "* Grids setup"
    grid_parameters%stratified = .false.
    call process_setup_grids (process, grid_parameters, calls=10000)
    print *
    print *, "* 3 + 3 iterations"
    call process_results_write_header (process)
    do i = 1, 3
       discard_integrals = i==1
       adapt_grids = .true.
       adapt_weights = .true.
       print_current = .true.
       call process_integrate (process, rng, grid_parameters, &
            1, 1, 1, 10000, &
            discard_integrals, adapt_grids, adapt_weights, print_current, &
            time_estimate)
    end do
    call process_results_write_current_average (process)
    call process_integrate (process, rng, grid_parameters, &
         2, 1, 3, 10000, &
         .true., .true., .false., .true., .true.)
    call process_results_write_footer (process)
    call process_write_time_estimate (process)
    print *
    print *, "* Process written to 'fort.61'"
    call process_write (process, 61)
  end subroutine process_test1

  subroutine process_test2 (prc_lib, os_data, model, var_list)
    type(process_library_t), intent(inout) :: prc_lib
    type(model_t), intent(in), target :: model
    type(var_list_t), intent(in), target :: var_list
    type(lhapdf_status_t) :: lhapdf_status
    type(string_t) :: objlist
    type(process_t), pointer :: process
    type(os_data_t), intent(inout) :: os_data
    type(phs_parameters_t) :: phs_par
    type(mapping_defaults_t) :: mapping_defaults
    type(flavor_t), dimension(1) :: flv
    type(polarization_t), dimension(1) :: pol
    type(beam_data_t) :: beam_data
    type(grid_parameters_t) :: grid_parameters
    type(tao_random_state) :: rng
    real(default) :: weight
    logical :: time_estimate = .true.
    integer :: i
    logical :: rebuild_phs = .true.
    logical :: discard_integrals, adapt_grids, adapt_weights, print_current
    print *, "*** Test decay process Z -> u ubar ***"
    print *
    print *, "* Initialization"
    call tao_random_create (rng, 0)
    call process_store_init_process &
         (process, prc_lib, var_str ("zqq"), model, lhapdf_status, &
          var_list, use_beams=.false.)
    print *, "  Process ID = ", char (process%id)
    print *
    print *, "*** Beam/strfun setup (unpolarized)"
    print *
    call flavor_init (flv, (/ 23 /), model)
    call polarization_init_unpolarized (pol(1), flv(1))
    call beam_data_init_decay (beam_data, flv, pol)
    call process_setup_beams (process, beam_data, 0, 0)
    call process_connect_strfun (process)
    call process_setup_subevt (process)
    print *
    print *, "* Phase space setup"
    call openmp_set_num_threads_verbose (1) 
    call process_setup_phase_space (process, rebuild_phs, &
         os_data, phs_par, mapping_defaults, filename_out=var_str("zqq.phs"), &
         vis_channels = .false.)
    call process_init_vamp_history (process, 1)
    print *
    print *, "*** Test integration"
    print *, "* Grids setup"
    grid_parameters%stratified = .false.
    call process_setup_grids (process, grid_parameters, calls=9)
    print *
    print *, "* 1 iteration with minimal number of calls"
    call process_results_write_header (process)
    do i = 1, 1
       discard_integrals = i==1
       adapt_grids = .true.
       adapt_weights = .true.
       print_current = .true.
       call process_integrate (process, rng, grid_parameters, &
            1, 1, 1, 9, &
            discard_integrals, adapt_grids, adapt_weights, print_current, &
            time_estimate)
    end do
    call process_results_write_footer (process)
    call process_write_time_estimate (process)
    print *
    print *, "* Process written to 'fort.62'"
    call process_write (process, 62)
    print *
    print *, "*** Event generation"
    call process_setup_event_generation (process)
    print *
    print *, "* Generate weighted event"
    call process_generate_weighted_event (process, rng, weight)
    print *
    print *, "* Process written to 'fort.63'"
    call process_write (process, 63)
    print *, "weight =", weight
    print *
    print *, "* Generate unweighted event"
    call process_generate_unweighted_event (process, rng, weight)
    print *
    print *, "* Process written to 'fort.64'"
    call process_write (process, 64)
    print *, "excess weight =", weight
  end subroutine process_test2

  subroutine process_test3 (prc_lib, os_data, model, var_list)
    type(process_library_t), intent(inout) :: prc_lib
    type(model_t), intent(in), target :: model
    type(var_list_t), intent(in), target :: var_list
    type(lhapdf_status_t) :: lhapdf_status
    type(process_t), pointer :: process
    type(os_data_t), intent(inout) :: os_data
    type(phs_parameters_t) :: phs_par
    type(mapping_defaults_t) :: mapping_defaults
    type(flavor_t), dimension(2) :: flv
    type(polarization_t), dimension(2) :: pol
    type(beam_data_t) :: beam_data
    type(grid_parameters_t) :: grid_parameters
    real(default), dimension(:), allocatable :: x
    logical :: time_estimate = .true.
    integer :: channel
    logical :: ok
    integer :: i
    type(tao_random_state) :: rng
    logical :: rebuild_phs = .true.
    logical :: discard_integrals, adapt_grids, adapt_weights, print_current
    print *, "*** Test scattering process e+ e- -> nu nubar H ***"
    print *
    print *, "* Initialization"
    call tao_random_create (rng, 0)
    call process_store_init_process &
         (process, prc_lib, var_str ("nnh"), model, lhapdf_status, &
         var_list, use_beams = .true.)
    print *, "  Process ID = ", char (process%id)
    print *
    print *, "* Beam/strfun setup"
    print *
    call flavor_init (flv, (/ 11, -11 /), model)
    call polarization_init_unpolarized (pol(1), flv(1))
    call polarization_init_unpolarized (pol(2), flv(2))
    call beam_data_init_sqrts (beam_data, 500._default, flv, pol)
    call process_setup_beams (process, beam_data, 0, 0)
    call process_connect_strfun (process)
    call process_setup_subevt (process)
    print *
    print *, "* Phase space setup"
    call openmp_set_num_threads_verbose (1) 
    call process_setup_phase_space (process, rebuild_phs, &
         os_data, phs_par, mapping_defaults, filename_out=var_str("nnh.phs"), &
         vis_channels = .false.)
    call process_init_vamp_history (process, 8)
    print *
    print *, "* Kinematics setup"
    allocate (x (process_get_n_parameters (process)))
    do i = 1, size (x)
       x(i) = (i - 0.5_default) * (1._default / size (x))
    end do
    channel = 1
    call process_set_kinematics (process, x, channel, ok)
    print *
    print *, "* Process written to 'fort.70'"
    call process_write (process, 70)
    print *
    print *, "*** Test process evaluation"
    call process_evaluate (process)
    print *
    print *, "* Process written to 'fort.71'"
    call process_write (process, 71)
    print *
    print *, "*** Test integration"
    print *, "* Grids setup"
    call process_setup_grids (process, grid_parameters, calls=20000)
    print *
    print *, "* 5 + 3 iterations"
    call process_results_write_header (process)
    do i = 1, 5
       discard_integrals = i==1
       adapt_grids = .true.
       adapt_weights = .true.
       print_current = .true.
       call process_integrate (process, rng, grid_parameters, &
            1, 1, 1, 10000, &
            discard_integrals, adapt_grids, adapt_weights, print_current, &
            time_estimate)
    end do
    call process_results_write_current_average (process)
    call process_integrate (process, rng, grid_parameters, &
         2, 1, 3, 20000, .true., .false., .false., .true., .true.)
    call process_results_write_footer (process)
    call process_write_time_estimate (process)
    print *
    print *, "* Process written to 'fort.72'"
    call process_write (process, 72)
  end subroutine process_test3

  subroutine process_test4 (prc_lib, os_data, model, var_list)
    type(process_library_t), intent(inout) :: prc_lib
    type(model_t), intent(in), target :: model
    type(var_list_t), intent(in), target :: var_list
    type(lhapdf_status_t) :: lhapdf_status
    type(process_t), pointer :: process
    type(os_data_t), intent(inout) :: os_data
    type(phs_parameters_t) :: phs_par
    type(mapping_defaults_t) :: mapping_defaults
    type(flavor_t), dimension(2) :: flv
    type(polarization_t), dimension(2) :: pol
    type(beam_data_t) :: beam_data
    type(pdf_builtin_data_t), dimension(2) :: data
    type(stream_t), target :: stream
    type(parse_tree_t) :: parse_tree
    type(grid_parameters_t) :: grid_parameters
    logical :: time_estimate = .true.
    integer :: i
    type(tao_random_state) :: rng
    logical :: rebuild_phs = .true.
    print *, "*** Test process setup for g g -> u ubar ***"
    print *
    print *, "* Initialization"
    call tao_random_create (rng, 0)
    call process_store_init_process &
         (process, prc_lib, var_str ("gguu"), model, lhapdf_status, &
         var_list, use_beams = .true.)
    print *, "  Process ID = ", char (process%id)
    print *
    print *, "* Beam/strfun setup"
    print *
    !    call flavor_init (flv, (/ 21, 21 /), model)
    call flavor_init (flv, (/ PROTON, PROTON /), model)
    call polarization_init_unpolarized (pol(1), flv(1))
    call polarization_init_unpolarized (pol(2), flv(2))
    call beam_data_init_sqrts (beam_data, 14000._default, flv, pol)
    !     call process_setup_beams (process, beam_data, 0, 0)
    call process_setup_beams (process, beam_data, 2, 0)
    call pdf_builtin_init (data(1), model, flv(1), name = &
         var_str("cteq6l"), path = os_data%pdf_builtin_datapath)
    call pdf_builtin_init (data(2), model, flv(2), name = &
         var_str("cteq6l"), path = os_data%pdf_builtin_datapath)
    call process_set_strfun (process, 1, 1, data(1), 1) 
    call process_set_strfun (process, 2, 2, data(2), 1)
    call process_connect_strfun (process)
    call process_setup_subevt (process)
    print *
    print *, "* Phase space setup"
    call openmp_set_num_threads_verbose (1) 
    call process_setup_phase_space (process, rebuild_phs, &
         os_data, phs_par, mapping_defaults, filename_out=var_str("gguu.phs"), &
         vis_channels = .false.)
    call process_init_vamp_history (process, 18)
    print *
    print *, "* Cuts setup"
    call stream_init (stream, var_str ("all Pt > 50 GeV [u:d:U:D]"))
    call parse_tree_init_lexpr (parse_tree, stream, .true.)
    call process_setup_cuts (process, parse_tree_get_root_ptr (parse_tree))
    call parse_tree_final (parse_tree)
    call stream_final (stream)
    print *
    print *, "* Scale setup"
    call stream_init (stream, var_str ("1 TeV"))
    call parse_tree_init_expr (parse_tree, stream, .true.)
    call process_setup_fac_scale (process, parse_tree_get_root_ptr (parse_tree))
    call parse_tree_final (parse_tree)
    call stream_final (stream)
    print *
    print *, "*** Test integration"
    print *, "* Grids setup"
    call process_setup_grids (process, grid_parameters, calls=10000)
    print *
    print *, "* 5 + 3 iterations"
    call process_results_write_header (process)
    do i = 1, 5
       call process_integrate (process, rng, grid_parameters, &
            1, 1, 1, 50000, i==1, .true., i>2, .true., .true.)
    end do
    call process_results_write_current_average (process)
    call process_integrate (process, rng, grid_parameters, &
         2, 1, 3, 50000, .true., .false., .true., .true., .true.)
    call process_results_write_footer (process)
    call process_write_time_estimate (process)
    print *
    print *, "* Process written to 'fort.90'"
    call process_write (process, 90)
  end subroutine process_test4


end module processes
