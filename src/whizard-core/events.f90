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

module events
  
  use kinds, only: default !NODEP!
  use iso_varying_string, string_t => varying_string !NODEP!
  use file_utils !NODEP!
  use limits, only: FMT_12 !NODEP!
  use diagnostics !NODEP!
  use unit_tests
  use os_interface
  
  use ifiles
  use lexers
  use parser

  use subevents
  use variables
  use expressions
  use models

  use state_matrices
  use particles
  use interactions
  use subevt_expr
  use rng_base
  use process_libraries
  use processes
  use process_stacks
  use event_transforms
  use decays
  use shower

  implicit none
  private

  public :: event_normalization_mode
  public :: event_normalization_string
  public :: event_normalization_update
  public :: event_t
  public :: pacify
  public :: events_test

  integer, parameter, public :: NORM_UNDEFINED = 0
  integer, parameter, public :: NORM_UNIT = 1
  integer, parameter, public :: NORM_N_EVT = 2
  integer, parameter, public :: NORM_SIGMA = 3
  integer, parameter, public :: NORM_S_N = 4


  type :: event_config_t
     logical :: unweighted = .false.
     integer :: norm_mode = NORM_UNDEFINED
     integer :: factorization_mode = FM_IGNORE_HELICITY
     logical :: keep_correlations = .false.
     real(default) :: sigma = 1
     integer :: n = 1
     real(default) :: safety_factor = 1
     type(parse_node_t), pointer :: pn_selection => null ()
     type(parse_node_t), pointer :: pn_reweight => null ()
     type(parse_node_t), pointer :: pn_analysis => null ()
   contains
     procedure :: write => event_config_write
  end type event_config_t

  type :: event_t
     type(event_config_t) :: config
     type(process_t), pointer :: process => null ()
     type(process_instance_t), pointer :: instance => null ()
     class(rng_t), allocatable :: rng
     integer :: selected_i_mci = 0
     integer :: selected_i_term = 0
     integer :: selected_channel = 0
     logical :: is_complete = .false.
     class(evt_t), pointer :: transform_first => null ()
     class(evt_t), pointer :: transform_last => null ()
     logical :: particle_set_exists = .false.
     type(particle_set_t), pointer :: particle_set => null ()
     logical :: sqme_ref_is_known = .false.
     real(default) :: sqme_ref = 0
     logical :: sqme_prc_is_known = .false.
     real(default) :: sqme_prc = 0
     logical :: weight_ref_is_known = .false.
     real(default) :: weight_ref = 0
     logical :: weight_prc_is_known = .false.
     real(default) :: weight_prc = 0
     logical :: excess_prc_is_known = .false.
     real(default) :: excess_prc = 0
     integer :: n_alt = 0
     logical :: sqme_alt_is_known = .false.
     real(default), dimension(:), allocatable :: sqme_alt
     logical :: weight_alt_is_known = .false.
     real(default), dimension(:), allocatable :: weight_alt
     type(event_expr_t) :: expr
     logical :: selection_evaluated = .false.
     logical :: passed = .false.
     real(default) :: reweight = 1
     logical :: analysis_flag = .false.
   contains
     procedure :: final => event_final
     procedure :: write => event_write
     procedure :: basic_init => event_init
     procedure :: set_sigma => event_set_sigma
     procedure :: set_n => event_set_n
     procedure :: import_transform => event_import_transform
     procedure :: connect => event_connect
     procedure :: set_selection => event_set_selection
     procedure :: set_reweight => event_set_reweight
     procedure :: set_analysis => event_set_analysis
     procedure :: setup_expressions => event_setup_expressions
     procedure :: evaluate_transforms => event_evaluate_transforms
     procedure :: evaluate_expressions => event_evaluate_expressions
     procedure :: store_alt_values => event_store_alt_values
     procedure :: reset => event_reset
     procedure :: set => event_set
     procedure :: import_instance_results => event_import_instance_results
     procedure :: accept_sqme_ref => event_accept_sqme_ref
     procedure :: accept_sqme_prc => event_accept_sqme_prc
     procedure :: accept_weight_ref => event_accept_weight_ref
     procedure :: accept_weight_prc => event_accept_weight_prc
     procedure :: update_normalization => event_update_normalization
     procedure :: check => event_check
     procedure :: generate => event_generate
     procedure :: get_particle_set_hard_proc => event_get_particle_set_hard_proc
     procedure :: select => event_select
     procedure :: set_particle_set_hard_proc => event_set_particle_set_hard_proc
     procedure :: accept_particle_set => event_accept_particle_set
     procedure :: recalculate => event_recalculate
     procedure :: get_process_ptr => event_get_process_ptr
     procedure :: has_particle_set => event_has_particle_set
     procedure :: get_particle_set_ptr => event_get_particle_set_ptr
     procedure :: get_i_mci => event_get_i_mci
     procedure :: get_i_term => event_get_i_term
     procedure :: get_channel => event_get_channel
     procedure :: get_norm_mode => event_get_norm_mode
     procedure :: get_kinematical_weight => event_get_kinematical_weight
     procedure :: get_fac_scale => event_get_fac_scale
     procedure :: get_alpha_s => event_get_alpha_s
  end type event_t


  interface pacify
     module procedure pacify_event
  end interface pacify

contains
  
  subroutine event_config_write (object, unit, show_expressions)
    class(event_config_t), intent(in) :: object
    integer, intent(in), optional :: unit
    logical, intent(in), optional :: show_expressions
    integer :: u
    u = output_unit (unit)
    write (u, "(3x,A,L1)")  "Unweighted         = ", object%unweighted
    write (u, "(3x,A,A)")   "Normalization      = ", &
         char (event_normalization_string (object%norm_mode))
    write (u, "(3x,A)", advance="no")  "Helicity handling  = "
    select case (object%factorization_mode)
    case (FM_IGNORE_HELICITY)
       write (u, "(A)")  "drop"
    case (FM_SELECT_HELICITY)
       write (u, "(A)")  "select"
    case (FM_FACTOR_HELICITY)
       write (u, "(A)")  "factorize"
    end select
    write (u, "(3x,A,L1)")  "Keep correlations  = ", object%keep_correlations
    if (object%safety_factor /= 1) then
       write (u, "(3x,A," // FMT_12 // ")")  &
            "Safety factor      = ", object%safety_factor
    end if
    if (present (show_expressions)) then
       if (show_expressions) then
          if (associated (object%pn_selection)) then
             call write_separator (u)
             write (u, "(3x,A)") "Event selection expression:"
             call object%pn_selection%write (u)
          end if
          if (associated (object%pn_reweight)) then
             call write_separator (u)
             write (u, "(3x,A)") "Event reweighting expression:"
             call object%pn_reweight%write (u)
          end if
          if (associated (object%pn_analysis)) then
             call write_separator (u)
             write (u, "(3x,A)") "Analysis expression:"
             call object%pn_analysis%write (u)
          end if
       end if
    end if
  end subroutine event_config_write
  
  function event_normalization_mode (string, unweighted) result (mode)
    integer :: mode
    type(string_t), intent(in) :: string
    logical, intent(in) :: unweighted
    select case (lower_case (char (string)))
    case ("auto")
       if (unweighted) then
          mode = NORM_UNIT
       else
          mode = NORM_SIGMA
       end if
    case ("1")
       mode = NORM_UNIT
    case ("1/n")
       mode = NORM_N_EVT
    case ("sigma")
       mode = NORM_SIGMA
    case ("sigma/n")
       mode = NORM_S_N
    case default
       call msg_fatal ("Event normalization: unknown value '" &
            // char (string) // "'")
    end select
  end function event_normalization_mode
  
  function event_normalization_string (norm_mode) result (string)
    integer, intent(in) :: norm_mode
    type(string_t) :: string
    select case (norm_mode)
    case (NORM_UNDEFINED); string = "[undefined]"
    case (NORM_UNIT);      string = "'1'"
    case (NORM_N_EVT);     string = "'1/n'"
    case (NORM_SIGMA);     string = "'sigma'"
    case (NORM_S_N);       string = "'sigma/n'"
    case default;          string = "???"
    end select
  end function event_normalization_string
  
  subroutine event_normalization_update (weight, sigma, n, mode_new, mode_old)
    real(default), intent(inout) :: weight
    real(default), intent(in) :: sigma
    integer, intent(in) :: n
    integer, intent(in) :: mode_new, mode_old
    if (mode_new /= mode_old) then
       if (sigma > 0 .and. n > 0) then
          weight = weight / factor (mode_old) * factor (mode_new)
       else
          call msg_fatal ("Event normalization update: null sample")
       end if
    end if
  contains
    function factor (mode)
      real(default) :: factor
      integer, intent(in) :: mode
      select case (mode)
      case (NORM_UNIT);   factor = 1._default
      case (NORM_N_EVT);  factor = 1._default / n
      case (NORM_SIGMA);  factor = sigma
      case (NORM_S_N);    factor = sigma / n
      case default
         call msg_fatal ("Event normalization update: undefined mode")
      end select
    end function factor
  end subroutine event_normalization_update
  
  subroutine event_final (object)
    class(event_t), intent(inout) :: object
    class(evt_t), pointer :: evt
    if (allocated (object%rng))  call object%rng%final ()
    call object%expr%final ()
    do while (associated (object%transform_first))
       evt => object%transform_first
       object%transform_first => evt%next
       call evt%final ()
       deallocate (evt)
    end do
  end subroutine event_final
    
  subroutine event_write (object, unit, show_process, show_transforms, &
       show_decay, verbose, testflag)
    class(event_t), intent(in) :: object
    integer, intent(in), optional :: unit
    logical, intent(in), optional :: show_process, show_transforms, show_decay
    logical, intent(in), optional :: verbose
    logical, intent(in), optional :: testflag
    logical :: prc, trans, dec, verb
    class(evt_t), pointer :: evt
    integer :: u, i
    u = output_unit (unit)
    prc = .true.;  if (present (show_process))  prc = show_process
    trans = .true.;  if (present (show_transforms))  trans = show_transforms
    dec = .true.;  if (present (show_decay))  dec = show_decay
    verb = .false.;  if (present (verbose))  verb = verbose
    call write_separator_double (u)
    if (object%is_complete) then
       write (u, "(1x,A)")  "Event"
    else
       write (u, "(1x,A)")  "Event [incomplete]"
    end if
    call write_separator (u)
    call object%config%write (u)
    if (object%sqme_ref_is_known .or. object%weight_ref_is_known) then
       call write_separator (u)
    end if
    if (object%sqme_ref_is_known) then
       write (u, "(3x,A,ES19.12)")  "Squared matrix el. = ", object%sqme_ref
       if (object%sqme_alt_is_known) then
          do i = 1, object%n_alt
             write (u, "(5x,A,ES19.12,1x,I0)")  &
                  "alternate sqme   = ", object%sqme_alt(i), i
          end do
       end if
    end if
    if (object%weight_ref_is_known) then
       write (u, "(3x,A,ES19.12)")  "Event weight       = ", object%weight_ref
       if (object%weight_alt_is_known) then
          do i = 1, object%n_alt
             write (u, "(5x,A,ES19.12,1x,I0)")  &
                  "alternate weight = ", object%weight_alt(i), i
          end do
       end if
    end if
    if (object%selected_i_mci /= 0) then
       call write_separator (u)
       write (u, "(3x,A,I0)")  "Selected MCI group = ", object%selected_i_mci
       write (u, "(3x,A,I0)")  "Selected term      = ", object%selected_i_term
       write (u, "(3x,A,I0)")  "Selected channel   = ", object%selected_channel
    end if
    if (object%selection_evaluated) then
       call write_separator (u)
       write (u, "(3x,A,L1)")  "Passed selection   = ", object%passed
       if (object%passed) then
          write (u, "(3x,A,ES19.12)") &
               "Reweighting factor = ", object%reweight
          write (u, "(3x,A,L1)") &
               "Analysis flag      = ", object%analysis_flag
       end if
    end if
    if (associated (object%instance)) then
       if (prc) then
          if (verb) then
             call object%instance%write (u, testflag)
          else
             call object%instance%write_header (u)
          end if
       end if
       if (trans) then
          evt => object%transform_first
          do while (associated (evt))
             select type (evt)
             type is (evt_trivial_t)
                call evt%write (u, verbose = verbose, testflag = testflag)
             type is (evt_decay_t)
                call evt%write (u, show_decay_tree = dec, &
                     show_processes = dec .and. verb, verbose = verb)
             type is (evt_shower_t)
                call evt%write (u)
             end select
             call write_separator_double (u)
             evt => evt%next
          end do
       else
          call write_separator_double (u)
       end if
       if (object%expr%subevt_filled) then
          call object%expr%write (u)
          call write_separator_double (u)
       end if
    else
       call write_separator_double (u)
       write (u, "(1x,A)")  "Process instance: [undefined]"
       call write_separator_double (u)
    end if
  end subroutine event_write

  subroutine event_init (event, var_list, n_alt)
    class(event_t), intent(out) :: event
    type(var_list_t), intent(in), optional :: var_list
    integer, intent(in), optional :: n_alt
    type(string_t) :: norm_string
    logical :: polarized_events
    if (present (var_list)) then
       event%config%unweighted = var_list_get_lval (var_list, &
            var_str ("?unweighted"))
       norm_string = var_list_get_sval (var_list, &
            var_str ("$sample_normalization"))
       event%config%norm_mode = &
            event_normalization_mode (norm_string, event%config%unweighted)
       polarized_events = &
            var_list_get_lval (var_list, var_str ("?polarized_events"))
       if (polarized_events) then
          event%config%factorization_mode = FM_SELECT_HELICITY
       else
          event%config%factorization_mode = FM_IGNORE_HELICITY
       end if
       if (event%config%unweighted) then
          event%config%safety_factor = var_list_get_rval (var_list, &
               var_str ("safety_factor"))
       end if
    else
       event%config%norm_mode = NORM_SIGMA
    end if
    if (present (n_alt)) then
       event%n_alt = n_alt
       allocate (event%sqme_alt (n_alt))
       allocate (event%weight_alt (n_alt))
       call event%expr%init (n_alt)
    end if
    allocate (evt_trivial_t :: event%transform_first)
    event%transform_last => event%transform_first
  end subroutine event_init
    
  elemental subroutine event_set_sigma (event, sigma)
    class(event_t), intent(inout) :: event
    real(default), intent(in) :: sigma
    event%config%sigma = sigma
  end subroutine event_set_sigma

  elemental subroutine event_set_n (event, n)
    class(event_t), intent(inout) :: event
    integer, intent(in) :: n
    event%config%n = n
  end subroutine event_set_n
  
  subroutine event_import_transform (event, evt)
    class(event_t), intent(inout) :: event
    class(evt_t), intent(inout), pointer :: evt
    event%transform_last%next => evt
    evt%previous => event%transform_last
    event%transform_last => evt
    evt => null ()
  end subroutine event_import_transform
    
  subroutine event_connect (event, process_instance, model, process_stack)
    class(event_t), intent(inout), target :: event
    type(process_instance_t), intent(in), target :: process_instance
    type(model_t), intent(in), target :: model
    type(process_stack_t), intent(in), optional :: process_stack
    type(string_t) :: id
    integer :: num_id
    class(evt_t), pointer :: evt
    event%process => process_instance%process
    event%instance => process_instance
    id = event%process%get_id ()
    if (id /= "")  call event%expr%set_process_id (id)
    num_id = event%process%get_num_id ()
    if (num_id /= 0)  call event%expr%set_process_num_id (num_id)
    call event%expr%setup_vars (event%process%get_sqrts ())
    call event%expr%link_var_list (event%process%get_var_list_ptr ())
    call event%process%make_rng (event%rng)
    evt => event%transform_first
    do while (associated (evt))
       call evt%connect (process_instance, model, process_stack)
       evt => evt%next
    end do
  end subroutine event_connect

  subroutine event_set_selection (event, pn_selection)
    class(event_t), intent(inout) :: event
    type(parse_node_t), intent(in), pointer :: pn_selection
    event%config%pn_selection => pn_selection
  end subroutine event_set_selection

  subroutine event_set_reweight (event, pn_reweight)
    class(event_t), intent(inout) :: event
    type(parse_node_t), intent(in), pointer :: pn_reweight
    event%config%pn_reweight => pn_reweight
  end subroutine event_set_reweight

  subroutine event_set_analysis (event, pn_analysis)
    class(event_t), intent(inout) :: event
    type(parse_node_t), intent(in), pointer :: pn_analysis
    event%config%pn_analysis => pn_analysis
  end subroutine event_set_analysis
  
  subroutine event_setup_expressions (event)
    class(event_t), intent(inout), target :: event
    call event%expr%setup_selection (event%config%pn_selection)
    call event%expr%setup_analysis (event%config%pn_analysis)
    call event%expr%setup_reweight (event%config%pn_reweight)
  end subroutine event_setup_expressions
  
  subroutine event_evaluate_transforms (event, r)
    class(event_t), intent(inout) :: event
    real(default), dimension(:), intent(in), optional :: r
    class(evt_t), pointer :: evt
    integer :: i_term
    event%particle_set_exists = .false.
    call event%check ()
    if (event%instance%is_complete_event ()) then
       call event%instance%select_i_term (i_term)
       event%selected_i_term = i_term
       evt => event%transform_first
       do while (associated (evt))
          call evt%prepare_new_event &
               (event%selected_i_mci, event%selected_i_term)
          evt => evt%next
       end do
       evt => event%transform_first
       do while (associated (evt))
          call evt%generate_unweighted ()
          if (signal_is_pending ())  return
          call evt%make_particle_set (event%config%factorization_mode, &
               event%config%keep_correlations)
          if (signal_is_pending ())  return
          if (.not. evt%particle_set_exists)  exit
          evt => evt%next
       end do
       evt => event%transform_last
       if (associated (evt) .and. evt%particle_set_exists) then
          event%particle_set => evt%particle_set
          call event%accept_particle_set ()
       end if
    end if
  end subroutine event_evaluate_transforms
    
  subroutine event_evaluate_expressions (event)
    class(event_t), intent(inout) :: event
    if (event%particle_set_exists) then
       call event%expr%fill_subevt (event%particle_set)
    end if
    if (event%weight_ref_is_known) then
       call event%expr%set (weight_ref = event%weight_ref)
    end if
    if (event%weight_prc_is_known) then
       call event%expr%set (weight_prc = event%weight_prc)
    end if
    if (event%excess_prc_is_known) then
       call event%expr%set (excess_prc = event%excess_prc)
    end if
    if (event%sqme_ref_is_known) then
       call event%expr%set (sqme_ref = event%sqme_ref)
    end if
    if (event%sqme_prc_is_known) then
       call event%expr%set (sqme_prc = event%sqme_prc)
    end if
    if (event%particle_set_exists) then
       call event%expr%evaluate &
            (event%passed, event%reweight, event%analysis_flag)
       event%selection_evaluated = .true.
    end if
  end subroutine event_evaluate_expressions
  
  subroutine event_store_alt_values (event)
    class(event_t), intent(inout) :: event
    if (event%weight_alt_is_known) then
       call event%expr%set (weight_alt = event%weight_alt)
    end if
    if (event%sqme_alt_is_known) then
       call event%expr%set (sqme_alt = event%sqme_alt)
    end if
  end subroutine event_store_alt_values
  
  subroutine event_reset (event)
    class(event_t), intent(inout) :: event
    class(evt_t), pointer :: evt
    event%selected_i_mci = 0
    event%selected_i_term = 0
    event%selected_channel = 0
    event%is_complete = .false.
    event%particle_set_exists = .false.
    event%sqme_ref_is_known = .false.
    event%sqme_prc_is_known = .false.
    event%sqme_alt_is_known = .false.
    event%weight_ref_is_known = .false.
    event%weight_prc_is_known = .false.
    event%weight_alt_is_known = .false.
    event%excess_prc_is_known = .false.
    call event%expr%reset ()
    event%selection_evaluated = .false.
    event%passed = .false.
    event%analysis_flag = .false.
    if (associated (event%instance)) then
       call event%instance%reset (reset_mci = .true.)
    end if
    evt => event%transform_first
    do while (associated (evt))
       call evt%reset ()
       evt => evt%next
    end do
  end subroutine event_reset
  
  subroutine event_set (event, &
       weight_ref, weight_prc, weight_alt, &
       excess_prc, &
       sqme_ref, sqme_prc, sqme_alt)
    class(event_t), intent(inout) :: event
    real(default), intent(in), optional :: weight_ref, weight_prc
    real(default), intent(in), optional :: sqme_ref, sqme_prc
    real(default), dimension(:), intent(in), optional :: sqme_alt, weight_alt
    real(default), intent(in), optional :: excess_prc
    if (present (sqme_ref)) then
       event%sqme_ref_is_known = .true.
       event%sqme_ref = sqme_ref
    end if
    if (present (sqme_prc)) then
       event%sqme_prc_is_known = .true.
       event%sqme_prc = sqme_prc
    end if 
    if (present (sqme_alt)) then
       event%sqme_alt_is_known = .true.
       event%sqme_alt = sqme_alt
    end if
    if (present (weight_ref)) then
       event%weight_ref_is_known = .true.
       event%weight_ref = weight_ref
    end if
    if (present (weight_prc)) then
       event%weight_prc_is_known = .true.
       event%weight_prc = weight_prc
    end if
    if (present (weight_alt)) then
       event%weight_alt_is_known = .true.
       event%weight_alt = weight_alt
    end if
    if (present (excess_prc)) then
       event%excess_prc_is_known = .true.
       event%excess_prc = excess_prc
    end if
  end subroutine event_set
  
  subroutine event_import_instance_results (event)
    class(event_t), intent(inout) :: event
    if (associated (event%instance)) then
       if (event%instance%has_evaluated_trace ()) then
          call event%set ( &
               sqme_prc = event%instance%get_sqme (), &
               weight_prc = event%instance%get_weight (), &
               excess_prc = event%instance%get_excess () &
               )
       end if
    end if
  end subroutine event_import_instance_results
  
  subroutine event_accept_sqme_ref (event)
    class(event_t), intent(inout) :: event
    if (event%sqme_ref_is_known) then
       call event%set (sqme_prc = event%sqme_ref)
    end if
  end subroutine event_accept_sqme_ref
  
  subroutine event_accept_sqme_prc (event)
    class(event_t), intent(inout) :: event
    if (event%sqme_prc_is_known) then
       call event%set (sqme_ref = event%sqme_prc)
    end if
  end subroutine event_accept_sqme_prc
  
  subroutine event_accept_weight_ref (event)
    class(event_t), intent(inout) :: event
    if (event%weight_ref_is_known) then
       call event%set (weight_prc = event%weight_ref)
    end if
  end subroutine event_accept_weight_ref
  
  subroutine event_accept_weight_prc (event)
    class(event_t), intent(inout) :: event
    if (event%weight_prc_is_known) then
       call event%set (weight_ref = event%weight_prc)
    end if
  end subroutine event_accept_weight_prc
  
  subroutine event_update_normalization (event, mode_ref)
    class(event_t), intent(inout) :: event
    integer, intent(in), optional :: mode_ref
    integer :: mode_old
    if (present (mode_ref)) then
       mode_old = mode_ref
    else if (event%config%unweighted) then
       mode_old = NORM_UNIT
    else
       mode_old = NORM_SIGMA
    end if
    call event_normalization_update (event%weight_prc, &
         event%config%sigma, event%config%n, &
         mode_new = event%config%norm_mode, &
         mode_old = mode_old)
    call event_normalization_update (event%excess_prc, &
         event%config%sigma, event%config%n, &
         mode_new = event%config%norm_mode, &
         mode_old = mode_old)
  end subroutine event_update_normalization
  
  subroutine event_check (event)
    class(event_t), intent(inout) :: event
    event%is_complete = event%particle_set_exists &
         .and. event%sqme_ref_is_known &
         .and. event%sqme_prc_is_known &
         .and. event%weight_ref_is_known &
         .and. event%weight_prc_is_known
    if (event%n_alt /= 0) then
       event%is_complete = event%is_complete &
            .and. event%sqme_alt_is_known &
            .and. event%weight_alt_is_known
    end if
  end subroutine event_check
  
  subroutine event_generate (event, i_mci, r)
    class(event_t), intent(inout) :: event
    integer, intent(in) :: i_mci
    real(default), dimension(:), intent(in), optional :: r
    call event%reset ()
    event%selected_i_mci = i_mci
    if (event%config%unweighted) then
       call event%process%generate_unweighted_event (event%instance, i_mci)
       if (signal_is_pending ()) return
       call event%instance%evaluate_event_data ()
       call event%instance%normalize_weight ()
    else
       call event%process%generate_weighted_event (event%instance, i_mci)
       if (signal_is_pending ()) return
       call event%instance%evaluate_event_data ()
    end if
    event%selected_channel = event%instance%get_channel ()
    call event%import_instance_results ()
    call event%accept_sqme_prc ()
    call event%update_normalization ()
    call event%accept_weight_prc ()
    call event%evaluate_transforms (r)
    if (signal_is_pending ())  return
    call event%check ()
  end subroutine event_generate
  
  subroutine event_get_particle_set_hard_proc (event, pset)
    class(event_t), intent(in) :: event
    type(particle_set_t), intent(out) :: pset
    class(evt_t), pointer :: evt
    evt => event%transform_first
    pset = evt%particle_set
  end subroutine event_get_particle_set_hard_proc
    
  subroutine event_select (event, i_mci, i_term, channel)
    class(event_t), intent(inout) :: event
    integer, intent(in) :: i_mci, i_term, channel
    if (associated (event%instance)) then
       event%selected_i_mci = i_mci
       event%selected_i_term = i_term
       event%selected_channel = channel
    else
       call msg_bug ("Event: select term: process instance undefined")
    end if
  end subroutine event_select

  subroutine event_set_particle_set_hard_proc (event, particle_set)
    class(event_t), intent(inout) :: event
    type(particle_set_t), intent(in) :: particle_set
    class(evt_t), pointer :: evt
    evt => event%transform_first
    call evt%set_particle_set (particle_set, &
         event%selected_i_mci, event%selected_i_term)
    event%particle_set => evt%particle_set
    call event%accept_particle_set ()
    evt => evt%next
    do while (associated (evt))
       call evt%reset ()
       evt => evt%next
    end do
  end subroutine event_set_particle_set_hard_proc

  subroutine event_accept_particle_set (event)
    class(event_t), intent(inout) :: event
    event%particle_set_exists = .true.
  end subroutine event_accept_particle_set
  
  subroutine event_recalculate &
       (event, update_sqme, weight_factor, recover_beams)
    class(event_t), intent(inout) :: event
    logical, intent(in) :: update_sqme
    real(default), intent(in), optional :: weight_factor
    logical, intent(in), optional :: recover_beams
    integer :: i_mci, i_term, channel
    if (event%particle_set_exists) then
       i_mci = event%selected_i_mci
       i_term = event%selected_i_term
       channel = event%selected_channel
       if (i_mci == 0 .or. i_term == 0 .or. channel == 0) then
          call msg_bug ("Event: recalculate: undefined selection parameters")
       end if
       call event%instance%choose_mci (i_mci)
       call event%instance%set_trace (event%particle_set, i_term, recover_beams)
       call event%instance%recover (channel, i_term, update_sqme) 
       if (signal_is_pending ())  return
       if (update_sqme .and. present (weight_factor)) then
          call event%instance%evaluate_event_data &
               (weight = event%instance%get_sqme () * weight_factor)
       else if (event%weight_ref_is_known) then
          call event%instance%evaluate_event_data &
               (weight = event%weight_ref)
       else
          call event%process%recover_event (event%instance, i_term)
          if (signal_is_pending ())  return
          call event%instance%evaluate_event_data ()
          if (event%config%unweighted) then
             call event%instance%normalize_weight ()
          end if
       end if
       if (signal_is_pending ())  return
       if (update_sqme) then
          call event%import_instance_results ()
       else
          call event%accept_sqme_ref ()
          call event%accept_weight_ref ()
       end if
    else
       call msg_bug ("Event: can't recalculate, particle set is undefined")
    end if
  end subroutine event_recalculate
  
  function event_get_process_ptr (event) result (ptr)
    class(event_t), intent(in) :: event
    type(process_t), pointer :: ptr
    ptr => event%process
  end function event_get_process_ptr

  function event_has_particle_set (event) result (flag)
    class(event_t), intent(in) :: event
    logical :: flag
    flag = event%particle_set_exists
  end function event_has_particle_set
  
  function event_get_particle_set_ptr (event) result (ptr)
    class(event_t), intent(in), target :: event
    type(particle_set_t), pointer :: ptr
    ptr => event%particle_set
  end function event_get_particle_set_ptr
  
  function event_get_i_mci (event) result (i_mci)
    class(event_t), intent(in) :: event
    integer :: i_mci
    i_mci = event%selected_i_mci
  end function event_get_i_mci
  
  function event_get_i_term (event) result (i_term)
    class(event_t), intent(in) :: event
    integer :: i_term
    i_term = event%selected_i_term
  end function event_get_i_term
  
  function event_get_channel (event) result (channel)
    class(event_t), intent(in) :: event
    integer :: channel
    channel = event%selected_channel
  end function event_get_channel
  
  elemental function event_get_norm_mode (event) result (norm_mode)
    class(event_t), intent(in) :: event
    integer :: norm_mode
    norm_mode = event%config%norm_mode
  end function event_get_norm_mode
  
  function event_get_kinematical_weight (event) result (f)
    class(event_t), intent(in) :: event
    real(default) :: f
    if (event%sqme_ref_is_known .and. event%weight_ref_is_known &
         .and. event%sqme_ref /= 0) then
       f = event%weight_ref / event%sqme_ref
    else
       f = 0
    end if
  end function event_get_kinematical_weight
    
  function event_get_fac_scale (event) result (fac_scale)
    class(event_t), intent(in) :: event
    real(default) :: fac_scale
    fac_scale = event%instance%get_fac_scale (event%selected_i_term)
  end function event_get_fac_scale
    
  function event_get_alpha_s (event) result (alpha_s)
    class(event_t), intent(in) :: event
    real(default) :: alpha_s
    alpha_s = event%instance%get_alpha_s (event%selected_i_term)
  end function event_get_alpha_s
    
  subroutine pacify_event (event)
    class(event_t), intent(inout) :: event
    class(evt_t), pointer :: evt
    if (event%particle_set_exists)  call pacify (event%particle_set)
    if (event%expr%subevt_filled)  call pacify (event%expr)
    evt => event%transform_first
    do while (associated (evt))
       select type (evt)
       type is (evt_decay_t);  call pacify (evt)
       end select
       evt => evt%next
    end do
  end subroutine pacify_event
  

  subroutine events_test (u, results)
    integer, intent(in) :: u
    type(test_results_t), intent(inout) :: results
    call test (events_1, "events_1", &
         "empty event record", &
         u, results)
    call test (events_2, "events_2", &
         "generate event", &
         u, results)
    call test (events_3, "events_3", &
         "expression evaluation", &
         u, results)
    call test (events_4, "events_4", &
         "recover event", &
         u, results)
    call test (events_5, "events_5", &
         "partially recover event", &
         u, results)
    call test (events_6, "events_6", &
         "decays", &
         u, results)
    call test (events_7, "events_7", &
         "decay options", &
         u, results)
  end subroutine events_test
  
  subroutine events_1 (u)
    integer, intent(in) :: u
    type(event_t), target :: event

    write (u, "(A)")  "* Test output: events_1"
    write (u, "(A)")  "*   Purpose: display an empty event object"
    write (u, "(A)")

    call event%write (u)
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: events_1"
    
  end subroutine events_1
  
  subroutine events_2 (u)
    integer, intent(in) :: u
    type(event_t), allocatable, target :: event
    type(process_t), allocatable, target :: process
    type(process_instance_t), allocatable, target :: process_instance
    type(model_list_t) :: model_list

    write (u, "(A)")  "* Test output: events_2"
    write (u, "(A)")  "*   Purpose: generate and display an event"
    write (u, "(A)")

    call syntax_model_file_init ()

    write (u, "(A)")  "* Generate test process event"

    allocate (process)
    allocate (process_instance)
    call prepare_test_process (process, process_instance, model_list)
    call process_instance%setup_event_data ()

    write (u, "(A)")
    write (u, "(A)")  "* Initialize event object"

    allocate (event)
    call event%basic_init ()
    call event%connect (process_instance, process%get_model_ptr ())
    
    write (u, "(A)")
    write (u, "(A)")  "* Generate test process event"

    call process%generate_weighted_event (process_instance, 1)

    write (u, "(A)")
    write (u, "(A)")  "* Fill event object"
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
    
    call model_list%final ()
    call syntax_model_file_final ()
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: events_2"
    
  end subroutine events_2
  
  subroutine events_3 (u)
    integer, intent(in) :: u
    type(string_t) :: expr_text
    type(ifile_t) :: ifile
    type(stream_t) :: stream
    type(parse_tree_t) :: pt_selection, pt_reweight, pt_analysis
    type(event_t), allocatable, target :: event
    type(process_t), allocatable, target :: process
    type(process_instance_t), allocatable, target :: process_instance
    type(model_list_t) :: model_list

    write (u, "(A)")  "* Test output: events_3"
    write (u, "(A)")  "*   Purpose: generate an event and evaluate expressions"
    write (u, "(A)")

    call syntax_model_file_init ()
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

    allocate (process)
    allocate (process_instance)
    call prepare_test_process (process, process_instance, model_list)
    call process%set_var_list &
         (model_get_var_list_ptr (process%get_model_ptr ()))
    call process_instance%setup_event_data ()

    write (u, "(A)")
    write (u, "(A)")  "* Initialize event object and set expressions"

    allocate (event)
    call event%basic_init ()

    call event%set_selection (parse_tree_get_root_ptr (pt_selection))
    call event%set_reweight (parse_tree_get_root_ptr (pt_reweight))
    call event%set_analysis (parse_tree_get_root_ptr (pt_analysis))
    
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
    
    call model_list%final ()
    call syntax_model_file_final ()
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: events_3"
    
  end subroutine events_3
  
  subroutine events_4 (u)
    integer, intent(in) :: u
    type(event_t), allocatable, target :: event
    type(process_t), allocatable, target :: process
    type(process_instance_t), allocatable, target :: process_instance
    type(particle_set_t) :: particle_set
    type(model_list_t) :: model_list

    write (u, "(A)")  "* Test output: events_4"
    write (u, "(A)")  "*   Purpose: generate and recover an event"
    write (u, "(A)")

    call syntax_model_file_init ()

    write (u, "(A)")  "* Generate test process event and save particle set"
    write (u, "(A)")

    allocate (process)
    allocate (process_instance)
    call prepare_test_process (process, process_instance, model_list)
    call process_instance%setup_event_data ()

    allocate (event)
    call event%basic_init ()
    call event%connect (process_instance, process%get_model_ptr ())
    
    call event%generate (1, [0.4_default, 0.4_default])
    call event%evaluate_expressions ()
    call event%write (u)
    
    particle_set = event%particle_set

    call event%final ()
    deallocate (event)

    call cleanup_test_process (process, process_instance)
    deallocate (process_instance)
    deallocate (process)

    write (u, "(A)")
    write (u, "(A)")  "* Recover event from particle set"
    write (u, "(A)")
    
    allocate (process)
    allocate (process_instance)
    call prepare_test_process (process, process_instance, model_list)
    call process_instance%setup_event_data ()

    allocate (event)
    call event%basic_init ()
    call event%connect (process_instance, process%get_model_ptr ())
    
    call event%select (1, 1, 1)
    call event%set_particle_set_hard_proc (particle_set)
    call event%recalculate (update_sqme = .true.)
    call event%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Transfer sqme and evaluate expressions"
    write (u, "(A)")
    
    call event%accept_sqme_prc ()
    call event%accept_weight_prc ()
    call event%check ()
    call event%evaluate_expressions ()
    call event%write (u)
    
    write (u, "(A)")
    write (u, "(A)")  "* Reset contents"
    write (u, "(A)")

    call event%reset ()
    event%transform_first%particle_set_exists = .false.
    call event%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"

    call particle_set_final (particle_set)

    call event%final ()
    deallocate (event)

    call cleanup_test_process (process, process_instance)
    deallocate (process_instance)
    deallocate (process)
    
    call model_list%final ()
    call syntax_model_file_final ()

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: events_4"
    
  end subroutine events_4
  
  subroutine events_5 (u)
    integer, intent(in) :: u
    type(event_t), allocatable, target :: event
    type(process_t), allocatable, target :: process
    type(process_instance_t), allocatable, target :: process_instance
    type(particle_set_t) :: particle_set
    real(default) :: sqme, weight
    type(model_list_t) :: model_list

    write (u, "(A)")  "* Test output: events_5"
    write (u, "(A)")  "*   Purpose: generate and recover an event"
    write (u, "(A)")

    call syntax_model_file_init ()

    write (u, "(A)")  "* Generate test process event and save particle set"
    write (u, "(A)")

    allocate (process)
    allocate (process_instance)
    call prepare_test_process (process, process_instance, model_list)
    call process_instance%setup_event_data ()

    allocate (event)
    call event%basic_init ()
    call event%connect (process_instance, process%get_model_ptr ())
    
    call event%generate (1, [0.4_default, 0.4_default])
    call event%evaluate_expressions ()
    call event%write (u)
    
    particle_set = event%particle_set
    sqme = event%sqme_ref
    weight = event%weight_ref

    call event%final ()
    deallocate (event)

    call cleanup_test_process (process, process_instance)
    deallocate (process_instance)
    deallocate (process)

    write (u, "(A)")
    write (u, "(A)")  "* Recover event from particle set"
    write (u, "(A)")
    
    allocate (process)
    allocate (process_instance)
    call prepare_test_process (process, process_instance, model_list)
    call process_instance%setup_event_data ()

    allocate (event)
    call event%basic_init ()
    call event%connect (process_instance, process%get_model_ptr ())
    
    call event%select (1, 1, 1)
    call event%set_particle_set_hard_proc (particle_set)
    call event%recalculate (update_sqme = .false.)
    call event%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Manually set sqme and evaluate expressions"
    write (u, "(A)")
    
    call event%set (sqme_ref = sqme, weight_ref = weight)
    call event%accept_sqme_ref ()
    call event%accept_weight_ref ()
    call event%evaluate_expressions ()
    call event%write (u)
    
    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"

    call particle_set_final (particle_set)

    call event%final ()
    deallocate (event)

    call cleanup_test_process (process, process_instance)
    deallocate (process_instance)
    deallocate (process)
    
    call model_list%final ()
    call syntax_model_file_final ()

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: events_5"
    
  end subroutine events_5
  
  subroutine events_6 (u)
    integer, intent(in) :: u
    type(os_data_t) :: os_data
    type(model_list_t) :: model_list
    type(model_t), pointer :: model
    type(string_t) :: prefix, procname1, procname2
    type(process_library_t), target :: lib
    type(process_stack_t) :: process_stack
    class(evt_t), pointer :: evt_decay
    type(event_t), allocatable, target :: event
    type(process_t), pointer :: process
    type(process_instance_t), allocatable, target :: process_instance

    write (u, "(A)")  "* Test output: events_6"
    write (u, "(A)")  "*   Purpose: generate an event with subsequent decays"
    write (u, "(A)")

    write (u, "(A)")  "* Generate test process and decay"
    write (u, "(A)")

    call syntax_model_file_init ()
    call os_data_init (os_data)

    prefix = "events_6"
    procname1 = prefix // "_p"
    procname2 = prefix // "_d"
    call prepare_testbed &
         (lib, process_stack, model_list, model, prefix, os_data, &
         scattering=.true., decay=.true.)

    write (u, "(A)")  "* Initialize decay process"

    call model_set_unstable (model, 25, [procname2])

    process => process_stack%get_process_ptr (procname1)
    allocate (process_instance)
    call process_instance%init (process)
    call process_instance%setup_event_data ()
    call process_instance%init_simulation (1)

    write (u, "(A)")
    write (u, "(A)")  "* Initialize event transform: decay"

    allocate (evt_decay_t :: evt_decay)
    call evt_decay%connect (process_instance, model, process_stack)
    
    write (u, "(A)")
    write (u, "(A)")  "* Initialize event object"
    write (u, "(A)")

    allocate (event)
    call event%basic_init ()
    call event%connect (process_instance, model)
    call event%import_transform (evt_decay)
    
    call event%write (u, show_decay = .true.)
    
    write (u, "(A)")
    write (u, "(A)")  "* Generate event"
    write (u, "(A)")

    call event%generate (1, [0.4_default, 0.4_default])
    call event%evaluate_expressions ()
    call event%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"

    call event%final ()
    deallocate (event)

    call process_instance%final ()
    deallocate (process_instance)

    call process_stack%final ()
    
    call model_list%final ()
    call syntax_model_file_final ()

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: events_6"
    
  end subroutine events_6
  
  subroutine events_7 (u)
    integer, intent(in) :: u
    type(os_data_t) :: os_data
    type(model_list_t) :: model_list
    type(model_t), pointer :: model
    type(string_t) :: prefix, procname2
    type(process_library_t), target :: lib
    type(process_stack_t) :: process_stack
    type(process_t), pointer :: process
    type(process_instance_t), allocatable, target :: process_instance

    write (u, "(A)")  "* Test output: events_7"
    write (u, "(A)")  "*   Purpose: check decay options"
    write (u, "(A)")

    write (u, "(A)")  "* Prepare test process"
    write (u, "(A)")

    call syntax_model_file_init ()
    call os_data_init (os_data)

    prefix = "events_7"
    procname2 = prefix // "_d"
    call prepare_testbed &
         (lib, process_stack, model_list, model, prefix, os_data, &
         scattering=.false., decay=.true.)

    write (u, "(A)")  "* Generate decay event, default options"
    write (u, "(A)")

    call model_set_unstable (model, 25, [procname2])

    process => process_stack%get_process_ptr (procname2)
    allocate (process_instance)
    call process_instance%init (process)
    call process_instance%setup_event_data (model)
    call process_instance%init_simulation (1)

    call process%generate_weighted_event (process_instance, 1)
    call process_instance%write (u)

    call process_instance%final ()
    deallocate (process_instance)

    write (u, "(A)")
    write (u, "(A)")  "* Generate decay event, helicity-diagonal decay"
    write (u, "(A)")

    call model_set_unstable (model, 25, [procname2], diagonal = .true.)

    process => process_stack%get_process_ptr (procname2)

    allocate (process_instance)
    call process_instance%init (process)
    call process_instance%setup_event_data (model)
    call process_instance%init_simulation (1)

    call process%generate_weighted_event (process_instance, 1)
    call process_instance%write (u)

    call process_instance%final ()
    deallocate (process_instance)

    write (u, "(A)")
    write (u, "(A)")  "* Generate decay event, isotropic decay, &
         &polarized final state"
    write (u, "(A)")

    call model_set_unstable (model, 25, [procname2], isotropic = .true.)
    call model_set_polarized (model, 6)
    call model_set_polarized (model, -6)

    process => process_stack%get_process_ptr (procname2)

    allocate (process_instance)
    call process_instance%init (process)
    call process_instance%setup_event_data (model)
    call process_instance%init_simulation (1)

    call process%generate_weighted_event (process_instance, 1)
    call process_instance%write (u)

    call process_instance%final ()
    deallocate (process_instance)

    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"
    
    call process_stack%final ()
    
    call model_list%final ()
    call syntax_model_file_final ()

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: events_7"
    
  end subroutine events_7
  

end module events
