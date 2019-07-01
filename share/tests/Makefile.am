## Makefile.am -- Makefile for WHIZARD tests
##
## Process this file with automake to produce Makefile.in
##
########################################################################
#
# Copyright (C) 1999-2014 by 
#     Wolfgang Kilian <kilian@physik.uni-siegen.de>
#     Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
#     Juergen Reuter <juergen.reuter@desy.de>
#     with contributions from
#     Christian Speckner <cnspeckn@googlemail.com>
#
# WHIZARD is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by 
# the Free Software Foundation; either version 2, or (at your option)
# any later version.
#
# WHIZARD is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the 
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
#
########################################################################

EXTRA_DIST = \
    $(TESTSUITE_MACROS) $(TESTSUITES_M4) $(TESTSUITES_SIN) \
    $(REF_OUTPUT_FILES) \
    structure_2_inc.sin testproc_3.phs \
    user_cuts.f90 user_strfun.f90 susyhit.in TEST.mdl

TESTSUITE_MACROS = testsuite.m4

REF_OUTPUT_FILES = \
    $(REF_OUTPUT_FILES_BASE) $(REF_OUTPUT_FILES_DOUBLE) \
    $(REF_OUTPUT_FILES_QUAD)

REF_OUTPUT_FILES_BASE = \
    ref-output/analysis_1.ref \
    ref-output/jets_1.ref \
    ref-output/pdg_arrays_1.ref \
    ref-output/pdg_arrays_2.ref \
    ref-output/pdg_arrays_3.ref \
    ref-output/pdg_arrays_4.ref \
    ref-output/pdg_arrays_5.ref \
    ref-output/expressions_1.ref \
    ref-output/expressions_2.ref \
    ref-output/expressions_3.ref \
    ref-output/expressions_4.ref \
    ref-output/beam_1.ref \
    ref-output/beam_2.ref \
    ref-output/beam_3.ref \
    ref-output/polarization_1.ref \
    ref-output/polarization_2.ref \
    ref-output/md5_1.ref \
    ref-output/cputime_1.ref \
    ref-output/cputime_2.ref \
    ref-output/lexer_1.ref \
    ref-output/sorting_1.ref \
    ref-output/state_matrix_1.ref \
    ref-output/state_matrix_2.ref \
    ref-output/state_matrix_3.ref \
    ref-output/interaction_1.ref \
    ref-output/parse_1.ref \
    ref-output/color_1.ref \
    ref-output/evaluator_1.ref \
    ref-output/evaluator_2.ref \
    ref-output/evaluator_3.ref \
    ref-output/os_interface_1.ref \
    ref-output/format_1.ref \
    ref-output/hepmc_interface_1.ref \
    ref-output/xml_1.ref \
    ref-output/xml_2.ref \
    ref-output/xml_3.ref \
    ref-output/xml_4.ref \
    ref-output/sm_qcd_1.ref \
    ref-output/models_1.ref \
    ref-output/models_2.ref \
    ref-output/models_3.ref \
    ref-output/models_4.ref \
    ref-output/auto_components_1.ref \
    ref-output/auto_components_2.ref \
    ref-output/auto_components_3.ref \
    ref-output/particles_1.ref \
    ref-output/particles_2.ref \
    ref-output/beam_structures_1.ref \
    ref-output/beam_structures_2.ref \
    ref-output/beam_structures_3.ref \
    ref-output/beam_structures_4.ref \
    ref-output/beam_structures_5.ref \
    ref-output/beam_structures_6.ref \
    ref-output/sf_aux_1.ref \
    ref-output/sf_aux_2.ref \
    ref-output/sf_aux_3.ref \
    ref-output/sf_mappings_1.ref \
    ref-output/sf_mappings_2.ref \
    ref-output/sf_mappings_3.ref \
    ref-output/sf_mappings_4.ref \
    ref-output/sf_mappings_5.ref \
    ref-output/sf_mappings_6.ref \
    ref-output/sf_mappings_7.ref \
    ref-output/sf_mappings_8.ref \
    ref-output/sf_mappings_9.ref \
    ref-output/sf_mappings_10.ref \
    ref-output/sf_mappings_11.ref \
    ref-output/sf_mappings_12.ref \
    ref-output/sf_mappings_13.ref \
    ref-output/sf_mappings_14.ref \
    ref-output/sf_base_1.ref \
    ref-output/sf_base_2.ref \
    ref-output/sf_base_3.ref \
    ref-output/sf_base_4.ref \
    ref-output/sf_base_5.ref \
    ref-output/sf_base_6.ref \
    ref-output/sf_base_7.ref \
    ref-output/sf_base_8.ref \
    ref-output/sf_base_9.ref \
    ref-output/sf_base_10.ref \
    ref-output/sf_base_11.ref \
    ref-output/sf_base_12.ref \
    ref-output/sf_base_13.ref \
    ref-output/sf_base_14.ref \
    ref-output/sf_pdf_builtin_1.ref \
    ref-output/sf_pdf_builtin_2.ref \
    ref-output/sf_pdf_builtin_3.ref \
    ref-output/sf_lhapdf5_1.ref \
    ref-output/sf_lhapdf5_2.ref \
    ref-output/sf_lhapdf5_3.ref \
    ref-output/sf_lhapdf6_1.ref \
    ref-output/sf_lhapdf6_2.ref \
    ref-output/sf_lhapdf6_3.ref \
    ref-output/sf_isr_1.ref \
    ref-output/sf_isr_2.ref \
    ref-output/sf_isr_3.ref \
    ref-output/sf_isr_4.ref \
    ref-output/sf_epa_1.ref \
    ref-output/sf_epa_2.ref \
    ref-output/sf_epa_3.ref \
    ref-output/sf_epa_4.ref \
    ref-output/sf_epa_5.ref \
    ref-output/sf_ewa_1.ref \
    ref-output/sf_ewa_2.ref \
    ref-output/sf_ewa_3.ref \
    ref-output/sf_ewa_4.ref \
    ref-output/sf_ewa_5.ref \
    ref-output/sf_circe1_1.ref \
    ref-output/sf_circe1_2.ref \
    ref-output/sf_circe1_3.ref \
    ref-output/sf_circe2_1.ref \
    ref-output/sf_circe2_2.ref \
    ref-output/sf_circe2_3.ref \
    ref-output/sf_beam_events_1.ref \
    ref-output/sf_beam_events_2.ref \
    ref-output/sf_escan_1.ref \
    ref-output/sf_escan_2.ref \
    ref-output/phs_base_1.ref \
    ref-output/phs_base_2.ref \
    ref-output/phs_base_3.ref \
    ref-output/phs_base_4.ref \
    ref-output/phs_base_5.ref \
    ref-output/phs_single_1.ref \
    ref-output/phs_single_2.ref \
    ref-output/phs_single_3.ref \
    ref-output/phs_single_4.ref \
    ref-output/phs_forest_1.ref \
    ref-output/phs_wood_1.ref \
    ref-output/phs_wood_2.ref \
    ref-output/phs_wood_3.ref \
    ref-output/phs_wood_4.ref \
    ref-output/phs_wood_5.ref \
    ref-output/phs_wood_6.ref \
    ref-output/phs_wood_vis_1.ref \
    ref-output/rng_base_1.ref \
    ref-output/rng_base_2.ref \
    ref-output/rng_tao_1.ref \
    ref-output/rng_tao_2.ref \
    ref-output/selectors_1.ref \
    ref-output/mci_base_1.ref \
    ref-output/mci_base_2.ref \
    ref-output/mci_base_3.ref \
    ref-output/mci_base_4.ref \
    ref-output/mci_base_5.ref \
    ref-output/mci_base_6.ref \
    ref-output/mci_base_7.ref \
    ref-output/mci_base_8.ref \
    ref-output/mci_midpoint_1.ref \
    ref-output/mci_midpoint_2.ref \
    ref-output/mci_midpoint_3.ref \
    ref-output/mci_midpoint_4.ref \
    ref-output/mci_midpoint_5.ref \
    ref-output/mci_midpoint_6.ref \
    ref-output/mci_midpoint_7.ref \
    ref-output/mci_vamp_1.ref \
    ref-output/mci_vamp_2.ref \
    ref-output/mci_vamp_3.ref \
    ref-output/mci_vamp_4.ref \
    ref-output/mci_vamp_5.ref \
    ref-output/mci_vamp_6.ref \
    ref-output/mci_vamp_7.ref \
    ref-output/mci_vamp_8.ref \
    ref-output/mci_vamp_9.ref \
    ref-output/mci_vamp_10.ref \
    ref-output/mci_vamp_11.ref \
    ref-output/mci_vamp_12.ref \
    ref-output/mci_vamp_13.ref \
    ref-output/mci_vamp_14.ref \
    ref-output/mci_vamp_15.ref \
    ref-output/prclib_interfaces_1.ref \
    ref-output/prclib_interfaces_2.ref \
    ref-output/prclib_interfaces_3.ref \
    ref-output/prclib_interfaces_4.ref \
    ref-output/prclib_interfaces_5.ref \
    ref-output/prclib_interfaces_6.ref \
    ref-output/prclib_interfaces_7.ref \
    ref-output/particle_specifiers_1.ref \
    ref-output/particle_specifiers_2.ref \
    ref-output/process_libraries_1.ref \
    ref-output/process_libraries_2.ref \
    ref-output/process_libraries_3.ref \
    ref-output/process_libraries_4.ref \
    ref-output/process_libraries_5.ref \
    ref-output/process_libraries_6.ref \
    ref-output/process_libraries_7.ref \
    ref-output/process_libraries_8.ref \
    ref-output/cascade_1.ref \
    ref-output/prclib_stacks_1.ref \
    ref-output/prclib_stacks_2.ref \
    ref-output/prc_test_1.ref \
    ref-output/prc_test_2.ref \
    ref-output/prc_test_3.ref \
    ref-output/prc_test_4.ref \
    ref-output/subevt_expr_1.ref \
    ref-output/subevt_expr_2.ref \
    ref-output/processes_1.ref \
    ref-output/processes_2.ref \
    ref-output/processes_3.ref \
    ref-output/processes_4.ref \
    ref-output/processes_5.ref \
    ref-output/processes_6.ref \
    ref-output/processes_7.ref \
    ref-output/processes_8.ref \
    ref-output/processes_9.ref \
    ref-output/processes_10.ref \
    ref-output/processes_11.ref \
    ref-output/processes_12.ref \
    ref-output/processes_13.ref \
    ref-output/processes_14.ref \
    ref-output/processes_15.ref \
    ref-output/processes_16.ref \
    ref-output/processes_17.ref \
    ref-output/process_stacks_1.ref \
    ref-output/process_stacks_2.ref \
    ref-output/process_stacks_3.ref \
    ref-output/slha_1.ref \
    ref-output/event_transforms_1.ref \
    ref-output/decays_1.ref \
    ref-output/decays_2.ref \
    ref-output/decays_3.ref \
    ref-output/decays_4.ref \
    ref-output/decays_5.ref \
    ref-output/decays_6.ref \
    ref-output/shower_1.ref \
    ref-output/shower_2.ref \
    ref-output/events_1.ref \
    ref-output/events_2.ref \
    ref-output/events_3.ref \
    ref-output/events_4.ref \
    ref-output/events_5.ref \
    ref-output/events_6.ref \
    ref-output/events_7.ref \
    ref-output/prc_template_me_1.ref \
    ref-output/prc_template_me_2.ref \
    ref-output/prc_omega_1.ref \
    ref-output/prc_omega_2.ref \
    ref-output/prc_omega_3.ref \
    ref-output/prc_omega_4.ref \
    ref-output/prc_omega_5.ref \
    ref-output/prc_omega_diags_1.ref \
    ref-output/eio_data_1.ref \
    ref-output/eio_data_2.ref \
    ref-output/eio_base_1.ref \
    ref-output/eio_raw_1.ref \
    ref-output/eio_raw_2.ref \
    ref-output/eio_checkpoints_1.ref \
    ref-output/eio_lhef_1.ref \
    ref-output/eio_lhef_2.ref \
    ref-output/eio_lhef_3.ref \
    ref-output/eio_lhef_4.ref \
    ref-output/eio_lhef_5.ref \
    ref-output/eio_lhef_6.ref \
    ref-output/eio_hepmc_1.ref \
    ref-output/eio_stdhep_1.ref \
    ref-output/eio_stdhep_2.ref \
    ref-output/eio_ascii_1.ref \
    ref-output/eio_ascii_2.ref \
    ref-output/eio_ascii_3.ref \
    ref-output/eio_ascii_4.ref \
    ref-output/eio_ascii_5.ref \
    ref-output/eio_ascii_6.ref \
    ref-output/eio_ascii_7.ref \
    ref-output/eio_ascii_8.ref \
    ref-output/eio_ascii_9.ref \
    ref-output/eio_ascii_10.ref \
    ref-output/eio_weights_1.ref \
    ref-output/eio_weights_2.ref \
    ref-output/iterations_1.ref \
    ref-output/iterations_2.ref \
    ref-output/rt_data_1.ref \
    ref-output/rt_data_2.ref \
    ref-output/rt_data_3.ref \
    ref-output/rt_data_4.ref \
    ref-output/rt_data_5.ref \
    ref-output/rt_data_6.ref \
    ref-output/rt_data_7.ref \
    ref-output/rt_data_8.ref \
    ref-output/dispatch_1.ref \
    ref-output/dispatch_2.ref \
    ref-output/dispatch_3.ref \
    ref-output/dispatch_4.ref \
    ref-output/dispatch_5.ref \
    ref-output/dispatch_6.ref \
    ref-output/dispatch_7.ref \
    ref-output/dispatch_8.ref \
    ref-output/dispatch_9.ref \
    ref-output/dispatch_10.ref \
    ref-output/dispatch_11.ref \
    ref-output/dispatch_12.ref \
    ref-output/dispatch_13.ref \
    ref-output/dispatch_14.ref \
    ref-output/process_configurations_1.ref \
    ref-output/process_configurations_2.ref \
    ref-output/compilations_1.ref \
    ref-output/compilations_2.ref \
    ref-output/compilations_3.ref \
    ref-output/compilations_static_1.ref \
    ref-output/compilations_static_2.ref \
    ref-output/integrations_1.ref \
    ref-output/integrations_2.ref \
    ref-output/integrations_3.ref \
    ref-output/integrations_4.ref \
    ref-output/integrations_5.ref \
    ref-output/integrations_6.ref \
    ref-output/integrations_7.ref \
    ref-output/integrations_8.ref \
    ref-output/integrations_history_1.ref \
    ref-output/event_streams_1.ref \
    ref-output/event_streams_2.ref \
    ref-output/event_streams_3.ref \
    ref-output/event_streams_4.ref \
    ref-output/simulations_1.ref \
    ref-output/simulations_2.ref \
    ref-output/simulations_3.ref \
    ref-output/simulations_4.ref \
    ref-output/simulations_5.ref \
    ref-output/simulations_6.ref \
    ref-output/simulations_7.ref \
    ref-output/simulations_8.ref \
    ref-output/simulations_9.ref \
    ref-output/simulations_10.ref \
    ref-output/simulations_11.ref \
    ref-output/simulations_12.ref \
    ref-output/commands_1.ref \
    ref-output/commands_2.ref \
    ref-output/commands_3.ref \
    ref-output/commands_4.ref \
    ref-output/commands_5.ref \
    ref-output/commands_6.ref \
    ref-output/commands_7.ref \
    ref-output/commands_8.ref \
    ref-output/commands_9.ref \
    ref-output/commands_10.ref \
    ref-output/commands_11.ref \
    ref-output/commands_12.ref \
    ref-output/commands_13.ref \
    ref-output/commands_14.ref \
    ref-output/commands_15.ref \
    ref-output/commands_16.ref \
    ref-output/commands_17.ref \
    ref-output/commands_18.ref \
    ref-output/commands_19.ref \
    ref-output/commands_20.ref \
    ref-output/commands_21.ref \
    ref-output/commands_22.ref \
    ref-output/commands_23.ref \
    ref-output/commands_24.ref \
    ref-output/commands_25.ref \
    ref-output/commands_26.ref \
    ref-output/commands_27.ref \
    ref-output/commands_28.ref \
    ref-output/commands_29.ref \
    ref-output/commands_30.ref \
    ref-output/commands_31.ref \
    ref-output/commands_32.ref \
    ref-output/commands_33.ref \
    ref-output/structure_1.ref \
    ref-output/structure_2.ref \
    ref-output/structure_3.ref \
    ref-output/structure_4.ref \
    ref-output/structure_5.ref \
    ref-output/structure_6.ref \
    ref-output/structure_7.ref \
    ref-output/structure_8.ref \
    ref-output/vars.ref \
    ref-output/extpar.ref \
    ref-output/testproc_1.ref \
    ref-output/testproc_2.ref \
    ref-output/testproc_3.ref \
    ref-output/testproc_4.ref \
    ref-output/testproc_5.ref \
    ref-output/testproc_6.ref \
    ref-output/testproc_7.ref \
    ref-output/testproc_8.ref \
    ref-output/testproc_9.ref \
    ref-output/testproc_10.ref \
    ref-output/susyhit.ref \
    ref-output/restrictions.ref \
    ref-output/process_log.ref \
    ref-output/static_1.ref \
    ref-output/libraries_1.ref \
    ref-output/libraries_2.ref \
    ref-output/libraries_4.ref \
    ref-output/rebuild_2.ref \
    ref-output/rebuild_3.ref \
    ref-output/fatal.ref \
    ref-output/model_test.ref \
    ref-output/cuts.ref \
    ref-output/user_cuts.ref \
    ref-output/qedtest_1.ref \
    ref-output/qedtest_2.ref \
    ref-output/qedtest_5.ref \
    ref-output/qedtest_6.ref \
    ref-output/qedtest_7.ref \
    ref-output/qedtest_8.ref \
    ref-output/qedtest_9.ref \
    ref-output/qedtest_10.ref \
    ref-output/qcdtest_4.ref \
    ref-output/qcdtest_5.ref \
    ref-output/beam_setup_1.ref \
    ref-output/beam_setup_2.ref \
    ref-output/beam_setup_3.ref \
    ref-output/beam_setup_4.ref \
    ref-output/observables_1.ref \
    ref-output/event_weights_1.ref \
    ref-output/event_weights_2.ref \
    ref-output/reweight_1.ref \
    ref-output/reweight_2.ref \
    ref-output/reweight_3.ref \
    ref-output/reweight_4.ref \
    ref-output/reweight_5.ref \
    ref-output/reweight_6.ref \
    ref-output/reweight_7.ref \
    ref-output/analyze_1.ref \
    ref-output/analyze_2.ref \
    ref-output/analyze_3.ref \
    ref-output/analyze_4.ref \
    ref-output/colors.ref \
    ref-output/colors_hgg.ref \
    ref-output/alphas.ref \
    ref-output/jets_xsec.ref \
    ref-output/parton_shower_1.ref \
    ref-output/parton_shower_3.ref \
    ref-output/parton_shower_4.ref \
    ref-output/mlm_matching_fsr.ref \
    ref-output/mlm_matching_isr.ref \
    ref-output/hepmc_1.ref \
    ref-output/hepmc_2.ref \
    ref-output/hepmc_3.ref \
    ref-output/hepmc_4.ref \
    ref-output/hepmc_5.ref \
    ref-output/lhef_1.ref \
    ref-output/lhef_2.ref \
    ref-output/lhef_3.ref \
    ref-output/lhef_4.ref \
    ref-output/lhef_5.ref \
    ref-output/lhef_6.ref \
    ref-output/stdhep_1.ref \
    ref-output/stdhep_2.ref \
    ref-output/stdhep_3.ref \
    ref-output/fatal_beam_decay.ref \
    ref-output/smtest_1.ref \
    ref-output/smtest_3.ref \
    ref-output/smtest_4.ref \
    ref-output/smtest_5.ref \
    ref-output/smtest_6.ref \
    ref-output/smtest_7.ref \
    ref-output/smtest_9.ref \
    ref-output/smtest_10.ref \
    ref-output/smtest_11.ref \
    ref-output/smtest_12.ref \
    ref-output/smtest_13.ref \
    ref-output/smtest_14.ref \
    ref-output/smtest_15.ref \
    ref-output/mssmtest.ref \
    ref-output/show_1.ref \
    ref-output/show_2.ref \
    ref-output/show_3.ref \
    ref-output/show_4.ref \
    ref-output/multi_comp_4.ref \
    ref-output/flvsum_1.ref \
    ref-output/circe1_2.ref \
    ref-output/circe1_errors_1.ref \
    ref-output/circe2_1.ref \
    ref-output/circe2_2.ref \
    ref-output/ewa_4.ref \
    ref-output/beam_events_1.ref \
    ref-output/energy_scan_1.ref \
    ref-output/user_strfun.ref

# Reference files that depend on the numerical precision
REF_OUTPUT_FILES_DOUBLE = \
    ref-output-double/sf_isr_5.ref \
    ref-output-double/qedtest_3.ref \
    ref-output-double/qedtest_4.ref \
    ref-output-double/qcdtest_1.ref \
    ref-output-double/qcdtest_2.ref \
    ref-output-double/qcdtest_3.ref \
    ref-output-double/smtest_2.ref \
    ref-output-double/smtest_8.ref \
    ref-output-double/defaultcuts.ref \
    ref-output-double/parton_shower_2.ref \
    ref-output-double/helicity.ref \
    ref-output-double/lhef_7.ref \
    ref-output-double/multi_comp_1.ref \
    ref-output-double/multi_comp_2.ref \
    ref-output-double/multi_comp_3.ref \
    ref-output-double/pdf_builtin.ref \
    ref-output-double/lhapdf5.ref \
    ref-output-double/lhapdf6.ref \
    ref-output-double/ep_1.ref \
    ref-output-double/ep_2.ref \
    ref-output-double/ep_3.ref \
    ref-output-double/circe1_1.ref \
    ref-output-double/circe1_3.ref \
    ref-output-double/circe1_4.ref \
    ref-output-double/circe1_5.ref \
    ref-output-double/circe1_6.ref \
    ref-output-double/circe1_7.ref \
    ref-output-double/circe1_8.ref \
    ref-output-double/circe1_9.ref \
    ref-output-double/circe1_photons_1.ref \
    ref-output-double/circe1_photons_2.ref \
    ref-output-double/circe1_photons_3.ref \
    ref-output-double/circe1_photons_4.ref \
    ref-output-double/circe1_photons_5.ref \
    ref-output-double/ewa_1.ref \
    ref-output-double/ewa_2.ref \
    ref-output-double/ewa_3.ref \
    ref-output-double/ilc.ref \
    ref-output-double/beam_events_2.ref \
    ref-output-double/beam_events_3.ref

REF_OUTPUT_FILES_QUAD = \
    ref-output-quad/sf_isr_5.ref \
    ref-output-quad/qedtest_3.ref \
    ref-output-quad/qedtest_4.ref \
    ref-output-quad/qcdtest_1.ref \
    ref-output-quad/qcdtest_2.ref \
    ref-output-quad/qcdtest_3.ref \
    ref-output-quad/smtest_2.ref \
    ref-output-quad/smtest_8.ref \
    ref-output-quad/defaultcuts.ref \
    ref-output-quad/parton_shower_2.ref \
    ref-output-quad/helicity.ref \
    ref-output-quad/lhef_7.ref \
    ref-output-quad/multi_comp_1.ref \
    ref-output-quad/multi_comp_2.ref \
    ref-output-quad/multi_comp_3.ref \
    ref-output-quad/pdf_builtin.ref \
    ref-output-quad/lhapdf5.ref \
    ref-output-quad/lhapdf6.ref \
    ref-output-quad/ep_1.ref \
    ref-output-quad/ep_2.ref \
    ref-output-quad/ep_3.ref \
    ref-output-quad/circe1_1.ref \
    ref-output-quad/circe1_3.ref \
    ref-output-quad/circe1_4.ref \
    ref-output-quad/circe1_5.ref \
    ref-output-quad/circe1_6.ref \
    ref-output-quad/circe1_7.ref \
    ref-output-quad/circe1_8.ref \
    ref-output-quad/circe1_9.ref \
    ref-output-quad/circe1_photons_1.ref \
    ref-output-quad/circe1_photons_2.ref \
    ref-output-quad/circe1_photons_3.ref \
    ref-output-quad/circe1_photons_4.ref \
    ref-output-quad/circe1_photons_5.ref \
    ref-output-quad/ewa_1.ref \
    ref-output-quad/ewa_2.ref \
    ref-output-quad/ewa_3.ref \
    ref-output-quad/ilc.ref \
    ref-output-quad/beam_events_2.ref \
    ref-output-quad/beam_events_3.ref

TESTSUITES_M4 = \
    $(MISC_TESTS_M4) \
    $(MSSM_EXT_M4) \
    $(NMSSM_EXT_M4)

TESTSUITES_SIN = \
    $(UNIT_TESTS_SIN) \
    $(MISC_TESTS_SIN) \
    $(MSSM_EXT_SIN) \
    $(NMSSM_EXT_SIN)

MISC_TESTS_M4 =

UNIT_TESTS_SIN = \
    analysis.sin \
    jets.sin \
    pdg_arrays.sin \
    expressions.sin \
    beams.sin \
    polarizations.sin \
    sorting.sin \
    interactions.sin \
    state_matrices.sin \
    md5.sin \
    cputime.sin \
    color.sin \
    formats.sin \
    lexers.sin \
    parser.sin \
    evaluators.sin \
    os_interface.sin \
    xml.sin \
    sm_qcd.sin \
    models.sin \
    auto_components.sin \
    particles.sin \
    beam_structures.sin \
    sf_aux.sin \
    sf_mappings.sin \
    sf_base.sin \
    sf_pdf_builtin.sin \
    sf_lhapdf5.sin \
    sf_lhapdf6.sin \
    sf_isr.sin \
    sf_epa.sin \
    sf_ewa.sin \
    sf_circe1.sin \
    sf_circe2.sin \
    sf_beam_events.sin \
    sf_escan.sin \
    phs_base.sin \
    phs_single.sin \
    phs_forests.sin \
    phs_wood.sin \
    phs_wood_vis.sin \
    rng_base.sin \
    rng_tao.sin \
    selectors.sin \
    mci_base.sin \
    mci_midpoint.sin \
    mci_vamp.sin \
    prclib_interfaces.sin \
    particle_specifiers.sin \
    process_libraries.sin \
    prclib_stacks.sin \
    prc_test.sin \
    subevt_expr.sin \
    processes.sin \
    process_stacks.sin \
    cascades.sin \
    slha_interface.sin \
    event_transforms.sin \
    decays.sin \
    shower.sin \
    events.sin \
    prc_template_me.sin \
    prc_omega.sin \
    prc_omega_diags.sin \
    eio_data.sin \
    eio_base.sin \
    eio_raw.sin \
    eio_checkpoints.sin \
    eio_lhef.sin \
    eio_hepmc.sin \
    eio_stdhep.sin \
    eio_ascii.sin \
    eio_weights.sin \
    iterations.sin \
    rt_data.sin \
    dispatch.sin \
    process_configurations.sin \
    compilations.sin \
    compilations_static.sin \
    integrations.sin \
    integrations_history.sin \
    event_streams.sin \
    simulations.sin \
    commands.sin

MISC_TESTS_SIN = \
    empty.sin \
    fatal.sin \
    defaultcuts.sin \
    cuts.sin \
    model_test.sin \
    structure_1.sin \
    structure_2.sin \
    structure_3.sin \
    structure_4.sin \
    structure_5.sin \
    structure_6.sin \
    structure_7.sin \
    structure_8.sin \
    vars.sin \
    extpar.sin \
    testproc_1.sin \
    testproc_2.sin \
    testproc_3.sin \
    testproc_4.sin \
    testproc_5.sin \
    testproc_6.sin \
    testproc_7.sin \
    testproc_8.sin \
    testproc_9.sin \
    testproc_10.sin \
    libraries_1.sin \
    libraries_2.sin \
    libraries_3.sin \
    libraries_4.sin \
    rebuild_1.sin \
    rebuild_2.sin \
    rebuild_3.sin \
    qedtest_1.sin \
    qedtest_2.sin \
    qedtest_3.sin \
    qedtest_4.sin \
    qedtest_5.sin \
    qedtest_6.sin \
    qedtest_7.sin \
    qedtest_8.sin \
    qedtest_9.sin \
    qedtest_10.sin \
    beam_setup_1.sin \
    beam_setup_2.sin \
    beam_setup_3.sin \
    beam_setup_4.sin \
    qcdtest_1.sin \
    qcdtest_2.sin \
    qcdtest_3.sin \
    qcdtest_4.sin \
    qcdtest_5.sin \
    observables_1.sin \
    event_weights_1.sin \
    event_weights_2.sin \
    reweight_1.sin \
    reweight_2.sin \
    reweight_3.sin \
    reweight_4.sin \
    reweight_5.sin \
    reweight_6.sin \
    reweight_7.sin \
    analyze_1.sin \
    analyze_2.sin \
    analyze_3.sin \
    analyze_4.sin \
    colors.sin \
    colors_hgg.sin \
    alphas.sin \
    jets_xsec.sin \
    lhef_1.sin \
    lhef_2.sin \
    lhef_3.sin \
    lhef_4.sin \
    lhef_5.sin \
    lhef_6.sin \
    lhef_7.sin \
    lhef_8.sin \
    parton_shower_1.sin \
    parton_shower_2.sin \
    parton_shower_3.sin \
    parton_shower_4.sin \
    mlm_matching_fsr.sin \
    mlm_matching_isr.sin \
    hepmc.sin \
    hepmc_1.sin \
    hepmc_2.sin \
    hepmc_3.sin \
    hepmc_4.sin \
    hepmc_5.sin \
    stdhep_1.sin \
    stdhep_2.sin \
    stdhep_3.sin \
    fatal_beam_decay.sin \
    smtest_1.sin \
    smtest_2.sin \
    smtest_3.sin \
    smtest_4.sin \
    smtest_5.sin \
    smtest_6.sin \
    smtest_7.sin \
    smtest_8.sin \
    smtest_9.sin \
    smtest_10.sin \
    smtest_11.sin \
    smtest_12.sin \
    smtest_13.sin \
    smtest_14.sin \
    smtest_15.sin \
    mssmtest.sin \
    show_1.sin \
    show_2.sin \
    show_3.sin \
    show_4.sin \
    multi_comp_1.sin \
    multi_comp_2.sin \
    multi_comp_3.sin \
    multi_comp_4.sin \
    flvsum_1.sin \
    pdf_builtin.sin \
    lhapdf5.sin \
    lhapdf6.sin \
    ep_1.sin \
    ep_2.sin \
    ep_3.sin \
    circe1_1.sin \
    circe1_2.sin \
    circe1_3.sin \
    circe1_4.sin \
    circe1_5.sin \
    circe1_6.sin \
    circe1_7.sin \
    circe1_8.sin \
    circe1_9.sin \
    circe1_photons_1.sin \
    circe1_photons_2.sin \
    circe1_photons_3.sin \
    circe1_photons_4.sin \
    circe1_photons_5.sin \
    circe1_errors_1.sin \
    circe2_1.sin \
    circe2_2.sin \
    ewa_1.sin \
    ewa_2.sin \
    ewa_3.sin \
    ewa_4.sin \
    ilc.sin \
    beam_events_1.sin \
    beam_events_2.sin \
    beam_events_3.sin \
    energy_scan_1.sin \
    susyhit.sin \
    restrictions.sin \
    helicity.sin \
    process_log.sin \
    static_1.sin \
    static_1.exe.sin \
    user_cuts.sin \
    user_strfun.sin
### Extended self tests
#     sm_ext.sin

MSSM_EXT_M4 = \
    mssm_ext-ee.m4 mssm_ext-ee2.m4 \
    mssm_ext-en.m4 mssm_ext-tn.m4 \
    mssm_ext-uu.m4 mssm_ext-uu2.m4 mssm_ext-uuckm.m4 \
    mssm_ext-dd.m4 mssm_ext-dd2.m4 mssm_ext-ddckm.m4 \
    mssm_ext-bb.m4 mssm_ext-bt.m4 mssm_ext-tt.m4 \
    mssm_ext-ug.m4 mssm_ext-dg.m4 \
    mssm_ext-aa.m4 mssm_ext-wa.m4 mssm_ext-za.m4 \
    mssm_ext-ww.m4 mssm_ext-wz.m4 mssm_ext-zz.m4 \
    mssm_ext-gg.m4 mssm_ext-ga.m4 mssm_ext-gw.m4 mssm_ext-gz.m4

NMSSM_EXT_M4 = \
    nmssm_ext-aa.m4  nmssm_ext-bb1.m4 nmssm_ext-bb2.m4 \
    nmssm_ext-bt.m4  nmssm_ext-dd1.m4 \
    nmssm_ext-dd2.m4 nmssm_ext-ee1.m4 \
    nmssm_ext-ee2.m4 nmssm_ext-en.m4  \
    nmssm_ext-ga.m4  nmssm_ext-gg.m4  \
    nmssm_ext-gw.m4  nmssm_ext-gz.m4  \
    nmssm_ext-qg.m4  nmssm_ext-tn.m4  \
    nmssm_ext-tt1.m4 nmssm_ext-tt2.m4 \
    nmssm_ext-uu1.m4 nmssm_ext-uu2.m4 \
    nmssm_ext-wa.m4  nmssm_ext-ww1.m4 nmssm_ext-ww2.m4 \
    nmssm_ext-wz.m4  nmssm_ext-za.m4  \
    nmssm_ext-zz1.m4 nmssm_ext-zz2.m4

MSSM_EXT_SIN = $(MSSM_EXT_M4:.m4=.sin)
NMSSM_EXT_SIN = $(NMSSM_EXT_M4:.m4=.sin)

all-local: $(TESTSUITES_SIN)

if M4_AVAILABLE
SUFFIXES = .m4 .sin
.m4.sin: 
	$(M4) $(srcdir)/$(TESTSUITE_MACROS) $< > $@
endif M4_AVAILABLE

