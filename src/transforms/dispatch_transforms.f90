! WHIZARD 2.5.0 May 06 2017
!
! Copyright (C) 1999-2017 by
!     Wolfgang Kilian <kilian@physik.uni-siegen.de>
!     Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
!     Juergen Reuter <juergen.reuter@desy.de>
!
!     with contributions from
!     Fabian Bach <fabian.bach@t-online.de>
!     Bijan Chokoufe <bijan.chokoufe@desy.de>
!     Christian Speckner <cnspeckn@googlemail.com>
!     So Young Shim <soyoung.shim@desy.de>
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

module dispatch_transforms

  use iso_varying_string, string_t => varying_string
  use process
  use variables
  use system_defs, only: LF
  use system_dependencies, only: LHAPDF6_AVAILABLE
  use sf_lhapdf, only: lhapdf_initialize
  use diagnostics
  use models
  use os_interface
  use beam_structures

  use event_base, only: event_callback_t, event_callback_nop_t
  use eio_base
  use eio_raw
  use eio_checkpoints
  use eio_callback
  use eio_lhef
  use eio_hepmc
  use eio_lcio
  use eio_stdhep
  use eio_ascii
  use eio_weights
  use eio_dump

  use event_transforms
  use decays
  use shower_base
  use shower_core
  use shower
  use shower_pythia6
  use hadrons
  use mlm_matching
  use powheg_matching
  use ckkw_matching
  use tauola_interface !NODEP!
  use evt_nlo

  implicit none
  private

  public :: dispatch_evt_nlo
  public :: dispatch_evt_decay
  public :: dispatch_evt_shower
  public :: dispatch_matching
  public :: dispatch_evt_hadrons
  public :: dispatch_eio

contains

  subroutine dispatch_evt_nlo (evt, keep_failed_events)
    class(evt_t), intent(out), pointer :: evt
    logical, intent(in) :: keep_failed_events
    call msg_message ("Simulate: activating fixed-order NLO events")
    allocate (evt_nlo_t :: evt)
    evt%only_weighted_events = .true.
    select type (evt)
    type is (evt_nlo_t)
       evt%i_evaluation = 0
       evt%keep_failed_events = keep_failed_events
    end select
  end subroutine dispatch_evt_nlo

  subroutine dispatch_evt_decay (evt, var_list)
    class(evt_t), intent(out), pointer :: evt
    type(var_list_t), intent(in) :: var_list
    logical :: allow_decays
    allow_decays = var_list%get_lval (var_str ("?allow_decays"))
    if (allow_decays) then
       allocate (evt_decay_t :: evt)
       call msg_message ("Simulate: activating decays")
    else
       evt => null ()
    end if
  end subroutine dispatch_evt_decay

  subroutine dispatch_evt_shower (evt, var_list, model, fallback_model, &
       os_data, beam_structure, process)
    class(evt_t), intent(out), pointer :: evt
    type(var_list_t), intent(in) :: var_list
    type(model_t), pointer, intent(in) :: model, fallback_model
    type(os_data_t), intent(in) :: os_data
    type(beam_structure_t), intent(in) :: beam_structure
    type(process_t), intent(in), optional :: process
    type(string_t) :: lhapdf_file, lhapdf_dir, process_name
    integer :: lhapdf_member
    type(shower_settings_t) :: settings
    type(taudec_settings_t) :: taudec_settings
    call msg_message ("Simulate: activating parton shower")
    allocate (evt_shower_t :: evt)
    call settings%init (var_list)
    if (associated (model)) then
       call taudec_settings%init (var_list, model)
    else
       call taudec_settings%init (var_list, fallback_model)
    end if
    if (present (process)) then
       process_name = process%get_id ()
    else
       process_name = 'dispatch_testing'
    end if
    select type (evt)
    type is (evt_shower_t)
       call evt%init (fallback_model, os_data)
       lhapdf_member = &
            var_list%get_ival (var_str ("lhapdf_member"))
       if (LHAPDF6_AVAILABLE) then
          lhapdf_dir = &
               var_list%get_sval (var_str ("$lhapdf_dir"))
          lhapdf_file = &
               var_list%get_sval (var_str ("$lhapdf_file"))
          call lhapdf_initialize &
               (1, lhapdf_dir, lhapdf_file, lhapdf_member, evt%pdf_data%pdf)
       end if
       if (present (process))  call evt%pdf_data%setup ("Shower", &
            beam_structure, lhapdf_member, process%get_pdf_set ())
       select case (settings%method)
       case (PS_WHIZARD)
          allocate (shower_t :: evt%shower)
       case (PS_PYTHIA6)
          allocate (shower_pythia6_t :: evt%shower)
       case default
          call msg_fatal ('Shower: Method ' // &
            char (var_list%get_sval (var_str ("$shower_method"))) // &
            'not implemented!')
       end select
       call evt%shower%init (settings, taudec_settings, evt%pdf_data)
    end select
    call dispatch_matching (evt, settings, var_list, process_name)
  end subroutine dispatch_evt_shower

  subroutine dispatch_matching (evt, settings, var_list, process_name)
    class(evt_t), intent(inout) :: evt
    type(var_list_t), intent(in) :: var_list
    type(string_t), intent(in) :: process_name
    type(shower_settings_t), intent(in) :: settings
    select type (evt)
    type is (evt_shower_t)
       if (settings%mlm_matching .and. settings%ckkw_matching) then
          call msg_fatal ("Both MLM and CKKW matching activated," // &
               LF // "     aborting simulation")
       end if
       if (settings%powheg_matching) then
          call msg_message ("Simulate: applying POWHEG matching")
          allocate (powheg_matching_t :: evt%matching)
       end if
       if (settings%mlm_matching) then
          call msg_message ("Simulate: applying MLM matching")
          allocate (mlm_matching_t :: evt%matching)
       end if
       if (settings%ckkw_matching) then
          call msg_warning ("Simulate: CKKW(-L) matching not yet supported")
          allocate (ckkw_matching_t :: evt%matching)
       end if
       if (allocated (evt%matching)) &
            call evt%matching%init (var_list, process_name)
    end select
  end subroutine dispatch_matching

  subroutine dispatch_evt_hadrons (evt, var_list, fallback_model)
    class(evt_t), intent(out), pointer :: evt
    type(var_list_t), intent(in) :: var_list
    type(model_t), pointer, intent(in) :: fallback_model
    type(shower_settings_t) :: shower_settings
    type(hadron_settings_t) :: hadron_settings
    allocate (evt_hadrons_t :: evt)
    call msg_message ("Simulate: activating hadronization")
    call shower_settings%init (var_list)
    call hadron_settings%init (var_list)
    select type (evt)
    type is (evt_hadrons_t)
       call evt%init (fallback_model)
       select case (hadron_settings%method)
       case (HADRONS_WHIZARD)
          allocate (hadrons_hadrons_t :: evt%hadrons)
       case (HADRONS_PYTHIA6)
          allocate (hadrons_pythia6_t :: evt%hadrons)
       case (HADRONS_PYTHIA8)
          allocate (hadrons_pythia8_t :: evt%hadrons)
       case default
          call msg_fatal ('Hadronization: Method ' // &
            char (var_list%get_sval (var_str ("hadronization_method"))) // &
            'not implemented!')
       end select
       call evt%hadrons%init &
            (shower_settings, hadron_settings, fallback_model)
    end select
  end subroutine dispatch_evt_hadrons

  subroutine dispatch_eio (eio, method, var_list, fallback_model, &
         event_callback)
    class(eio_t), allocatable, intent(inout) :: eio
    type(string_t), intent(in) :: method
    type(var_list_t), intent(in) :: var_list
    type(model_t), target, intent(in) :: fallback_model
    class(event_callback_t), allocatable, intent(in) :: event_callback
    !!! !!! !!! Workaround for ifort v18(beta) bug
    type(event_callback_nop_t) :: event_callback_tmp
    logical :: check, keep_beams, keep_remnants, recover_beams
    logical :: use_alpha_s_from_file, use_scale_from_file
    logical :: write_sqme_prc, write_sqme_ref, write_sqme_alt
    logical :: output_cross_section, ensure_order
    type(string_t) :: lhef_version, lhef_extension, raw_version
    type(string_t) :: extension_default, debug_extension, dump_extension, &
         extension_hepmc, &
         extension_lha, extension_hepevt, extension_ascii_short, &
         extension_ascii_long, extension_athena, extension_mokka, &
         extension_stdhep, extension_stdhep_up, extension_stdhep_ev4, &
         extension_raw, extension_hepevt_verb, extension_lha_verb, &
         extension_lcio
    integer :: checkpoint
    logical :: show_process, show_transforms, show_decay, verbose, pacified
    logical :: dump_weights, dump_compressed, dump_summary, dump_screen
    keep_beams = &
         var_list%get_lval (var_str ("?keep_beams"))
    keep_remnants = &
         var_list%get_lval (var_str ("?keep_remnants"))
    ensure_order = &
         var_list%get_lval (var_str ("?hepevt_ensure_order"))
    recover_beams = &
         var_list%get_lval (var_str ("?recover_beams"))
    use_alpha_s_from_file = &
         var_list%get_lval (var_str ("?use_alpha_s_from_file"))
    use_scale_from_file = &
         var_list%get_lval (var_str ("?use_scale_from_file"))
    select case (char (method))
    case ("raw")
       allocate (eio_raw_t :: eio)
       select type (eio)
       type is (eio_raw_t)
          check = &
               var_list%get_lval (var_str ("?check_event_file"))
          raw_version = &
               var_list%get_sval (var_str ("$event_file_version"))
          extension_raw = &
               var_list%get_sval (var_str ("$extension_raw"))
          call eio%set_parameters (check, raw_version, extension_raw)
       end select
    case ("checkpoint")
       allocate (eio_checkpoints_t :: eio)
       select type (eio)
       type is (eio_checkpoints_t)
          checkpoint = &
               var_list%get_ival (var_str ("checkpoint"))
          pacified = &
               var_list%get_lval (var_str ("?pacify"))
          call eio%set_parameters (checkpoint, blank = pacified)
       end select
    case ("callback")
       allocate (eio_callback_t :: eio)
       select type (eio)
       type is (eio_callback_t)
          checkpoint = &
               var_list%get_ival (var_str ("event_callback_interval"))
          if (allocated (event_callback)) then
             call eio%set_parameters (event_callback, checkpoint)
          else
             !!! !!! !!! Workaround for ifort v18(beta) bug
             ! call eio%set_parameters (event_callback_nop_t (), 0)
             call eio%set_parameters (event_callback_tmp, 0)
          end if
       end select
    case ("lhef")
       allocate (eio_lhef_t :: eio)
       select type (eio)
       type is (eio_lhef_t)
          lhef_version = &
               var_list%get_sval (var_str ("$lhef_version"))
          lhef_extension = &
               var_list%get_sval (var_str ("$lhef_extension"))
          write_sqme_prc = &
               var_list%get_lval (var_str ("?lhef_write_sqme_prc"))
          write_sqme_ref = &
               var_list%get_lval (var_str ("?lhef_write_sqme_ref"))
          write_sqme_alt = &
               var_list%get_lval (var_str ("?lhef_write_sqme_alt"))
          call eio%set_parameters ( &
               keep_beams, keep_remnants, recover_beams, &
               use_alpha_s_from_file, use_scale_from_file, &
               char (lhef_version), lhef_extension, &
               write_sqme_ref, write_sqme_prc, write_sqme_alt)
       end select
    case ("hepmc")
       allocate (eio_hepmc_t :: eio)
       select type (eio)
       type is (eio_hepmc_t)
          output_cross_section = &
               var_list%get_lval (var_str ("?hepmc_output_cross_section"))
          extension_hepmc = &
               var_list%get_sval (var_str ("$extension_hepmc"))
          call eio%set_parameters (recover_beams, &
               use_alpha_s_from_file, use_scale_from_file, &
               extension_hepmc, output_cross_section)
       end select
    case ("lcio")
       allocate (eio_lcio_t :: eio)
       select type (eio)
       type is (eio_lcio_t)
          extension_lcio = &
               var_list%get_sval (var_str ("$extension_lcio"))
          call eio%set_parameters (recover_beams, &
               use_alpha_s_from_file, use_scale_from_file, &
               extension_lcio)
       end select
    case ("stdhep")
       allocate (eio_stdhep_hepevt_t :: eio)
       select type (eio)
       type is (eio_stdhep_hepevt_t)
          extension_stdhep = &
               var_list%get_sval (var_str ("$extension_stdhep"))
          call eio%set_parameters &
               (keep_beams, keep_remnants, ensure_order, recover_beams, &
                use_alpha_s_from_file, use_scale_from_file, extension_stdhep)
       end select
    case ("stdhep_up")
       allocate (eio_stdhep_hepeup_t :: eio)
       select type (eio)
       type is (eio_stdhep_hepeup_t)
          extension_stdhep_up = &
               var_list%get_sval (var_str ("$extension_stdhep_up"))
          call eio%set_parameters (keep_beams, keep_remnants, ensure_order, &
               recover_beams, use_alpha_s_from_file, &
               use_scale_from_file, extension_stdhep_up)
       end select
    case ("stdhep_ev4")
       allocate (eio_stdhep_hepev4_t :: eio)
       select type (eio)
       type is (eio_stdhep_hepev4_t)
          extension_stdhep_ev4 = &
               var_list%get_sval (var_str ("$extension_stdhep_ev4"))
          call eio%set_parameters &
               (keep_beams, keep_remnants, ensure_order, recover_beams, &
                use_alpha_s_from_file, use_scale_from_file, extension_stdhep_ev4)
       end select
    case ("ascii")
       allocate (eio_ascii_ascii_t :: eio)
       select type (eio)
       type is (eio_ascii_ascii_t)
          extension_default = &
               var_list%get_sval (var_str ("$extension_default"))
          call eio%set_parameters &
               (keep_beams, keep_remnants, ensure_order, extension_default)
       end select
    case ("athena")
       allocate (eio_ascii_athena_t :: eio)
       select type (eio)
       type is (eio_ascii_athena_t)
          extension_athena = &
               var_list%get_sval (var_str ("$extension_athena"))
          call eio%set_parameters &
               (keep_beams, keep_remnants, ensure_order, extension_athena)
       end select
    case ("debug")
       allocate (eio_ascii_debug_t :: eio)
       select type (eio)
       type is (eio_ascii_debug_t)
          debug_extension = &
               var_list%get_sval (var_str ("$debug_extension"))
          show_process = &
               var_list%get_lval (var_str ("?debug_process"))
          show_transforms = &
               var_list%get_lval (var_str ("?debug_transforms"))
          show_decay = &
               var_list%get_lval (var_str ("?debug_decay"))
          verbose = &
               var_list%get_lval (var_str ("?debug_verbose"))
          call eio%set_parameters ( &
               extension = debug_extension, &
               show_process = show_process, &
               show_transforms = show_transforms, &
               show_decay = show_decay, &
               verbose = verbose)
       end select
    case ("dump")
       allocate (eio_dump_t :: eio)
       select type (eio)
       type is (eio_dump_t)
          dump_extension = &
               var_list%get_sval (var_str ("$dump_extension"))
          pacified = &
               var_list%get_lval (var_str ("?pacify"))
          dump_weights = &
               var_list%get_lval (var_str ("?dump_weights"))
          dump_compressed = &
               var_list%get_lval (var_str ("?dump_compressed"))
          dump_summary = &
               var_list%get_lval (var_str ("?dump_summary"))
          dump_screen = &
               var_list%get_lval (var_str ("?dump_screen"))
          call eio%set_parameters ( &
               extension = dump_extension, &
               pacify = pacified, &
               weights = dump_weights, &
               compressed = dump_compressed, &
               summary = dump_summary, &
               screen = dump_screen)
       end select
    case ("hepevt")
       allocate (eio_ascii_hepevt_t :: eio)
       select type (eio)
       type is (eio_ascii_hepevt_t)
          extension_hepevt = &
               var_list%get_sval (var_str ("$extension_hepevt"))
          call eio%set_parameters &
               (keep_beams, keep_remnants, ensure_order, extension_hepevt)
       end select
    case ("hepevt_verb")
       allocate (eio_ascii_hepevt_verb_t :: eio)
       select type (eio)
       type is (eio_ascii_hepevt_verb_t)
          extension_hepevt_verb = &
               var_list%get_sval (var_str ("$extension_hepevt_verb"))
          call eio%set_parameters &
               (keep_beams, keep_remnants, ensure_order, extension_hepevt_verb)
       end select
    case ("lha")
       allocate (eio_ascii_lha_t :: eio)
       select type (eio)
       type is (eio_ascii_lha_t)
          extension_lha = &
               var_list%get_sval (var_str ("$extension_lha"))
          call eio%set_parameters &
               (keep_beams, keep_remnants, ensure_order, extension_lha)
       end select
    case ("lha_verb")
       allocate (eio_ascii_lha_verb_t :: eio)
       select type (eio)
       type is (eio_ascii_lha_verb_t)
          extension_lha_verb = var_list%get_sval ( &
               var_str ("$extension_lha_verb"))
          call eio%set_parameters &
               (keep_beams, keep_remnants, ensure_order, extension_lha_verb)
       end select
    case ("long")
       allocate (eio_ascii_long_t :: eio)
       select type (eio)
       type is (eio_ascii_long_t)
          extension_ascii_long = &
               var_list%get_sval (var_str ("$extension_ascii_long"))
          call eio%set_parameters &
               (keep_beams, keep_remnants, ensure_order, extension_ascii_long)
       end select
    case ("mokka")
       allocate (eio_ascii_mokka_t :: eio)
       select type (eio)
       type is (eio_ascii_mokka_t)
          extension_mokka = &
               var_list%get_sval (var_str ("$extension_mokka"))
          call eio%set_parameters &
               (keep_beams, keep_remnants, ensure_order, extension_mokka)
       end select
    case ("short")
       allocate (eio_ascii_short_t :: eio)
       select type (eio)
       type is (eio_ascii_short_t)
          extension_ascii_short = &
               var_list%get_sval (var_str ("$extension_ascii_short"))
          call eio%set_parameters &
               (keep_beams, keep_remnants, ensure_order, extension_ascii_short)
       end select
    case ("weight_stream")
       allocate (eio_weights_t :: eio)
       select type (eio)
       type is (eio_weights_t)
          pacified = &
               var_list%get_lval (var_str ("?pacify"))
          call eio%set_parameters (pacify = pacified)
       end select
    case default
       call msg_fatal ("Event I/O method '" // char (method) &
            // "' not implemented")
    end select
    call eio%set_fallback_model (fallback_model)
  end subroutine dispatch_eio


end module dispatch_transforms
