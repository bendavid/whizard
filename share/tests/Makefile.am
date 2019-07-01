## Makefile.am -- Makefile for WHIZARD tests
##
## Process this file with automake to produce Makefile.in
##
########################################################################
#
# Copyright (C) 1999-2016 by 
#     Wolfgang Kilian <kilian@physik.uni-siegen.de>
#     Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
#     Juergen Reuter <juergen.reuter@desy.de>
#     with contributions from
#     Fabian Bach <fabian.bach@t-online.de>
#     Bijan Chokoufe <bijan.chokoufe@desy.de>
#     Christian Speckner <cnspeckn@googlemail.com>
#     Marco Sekulla <marco.sekulla@kit.edu>
#     Christian Weiss <christian.weiss@desy.de>
#     Felix Braam, Sebastian Schmidt,
#     Hans-Werner Boschmann, Daniel Wiesler
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
    $(TESTSUITE_TOOLS) \
    $(REF_OUTPUT_FILES) \
    functional_tests/structure_2_inc.sin functional_tests/testproc_3.phs \
    functional_tests/user_cuts.f90 functional_tests/user_strfun.f90 \
    functional_tests/susyhit.in TEST.mdl nmssm.slha

TESTSUITE_MACROS = testsuite.m4
TESTSUITE_TOOLS = \
    check-debug-output.py \
    check-hepmc-weights.py \
    compare-integrals.py \
    compare-histograms.py

REF_OUTPUT_FILES = \
    extra_integration_results.dat \
    $(REF_OUTPUT_FILES_BASE) $(REF_OUTPUT_FILES_DOUBLE) \
    $(REF_OUTPUT_FILES_PREC) $(REF_OUTPUT_FILES_EXT) \
    $(REF_OUTPUT_FILES_QUAD)

REF_OUTPUT_FILES_BASE = \
    unit_tests/ref-output/analysis_1.ref \
    unit_tests/ref-output/pdg_arrays_1.ref \
    unit_tests/ref-output/pdg_arrays_2.ref \
    unit_tests/ref-output/pdg_arrays_3.ref \
    unit_tests/ref-output/pdg_arrays_4.ref \
    unit_tests/ref-output/pdg_arrays_5.ref \
    unit_tests/ref-output/expressions_1.ref \
    unit_tests/ref-output/expressions_2.ref \
    unit_tests/ref-output/expressions_3.ref \
    unit_tests/ref-output/expressions_4.ref \
    unit_tests/ref-output/su_algebra_1.ref \
    unit_tests/ref-output/su_algebra_2.ref \
    unit_tests/ref-output/su_algebra_3.ref \
    unit_tests/ref-output/su_algebra_4.ref \
    unit_tests/ref-output/bloch_vectors_1.ref \
    unit_tests/ref-output/bloch_vectors_2.ref \
    unit_tests/ref-output/bloch_vectors_3.ref \
    unit_tests/ref-output/bloch_vectors_4.ref \
    unit_tests/ref-output/bloch_vectors_5.ref \
    unit_tests/ref-output/bloch_vectors_6.ref \
    unit_tests/ref-output/bloch_vectors_7.ref \
    unit_tests/ref-output/polarization_1.ref \
    unit_tests/ref-output/polarization_2.ref \
    unit_tests/ref-output/beam_1.ref \
    unit_tests/ref-output/beam_2.ref \
    unit_tests/ref-output/beam_3.ref \
    unit_tests/ref-output/md5_1.ref \
    unit_tests/ref-output/cputime_1.ref \
    unit_tests/ref-output/cputime_2.ref \
    unit_tests/ref-output/lexer_1.ref \
    unit_tests/ref-output/parse_1.ref \
    unit_tests/ref-output/color_1.ref \
    unit_tests/ref-output/os_interface_1.ref \
    unit_tests/ref-output/evaluator_1.ref \
    unit_tests/ref-output/evaluator_2.ref \
    unit_tests/ref-output/evaluator_3.ref \
    unit_tests/ref-output/evaluator_4.ref \
    unit_tests/ref-output/format_1.ref \
    unit_tests/ref-output/sorting_1.ref \
    unit_tests/ref-output/grids_1.ref \
    unit_tests/ref-output/grids_2.ref \
    unit_tests/ref-output/grids_3.ref \
    unit_tests/ref-output/grids_4.ref \
    unit_tests/ref-output/grids_5.ref \
    unit_tests/ref-output/solver_1.ref \
    unit_tests/ref-output/state_matrix_1.ref \
    unit_tests/ref-output/state_matrix_2.ref \
    unit_tests/ref-output/state_matrix_3.ref \
    unit_tests/ref-output/state_matrix_4.ref \
    unit_tests/ref-output/state_matrix_5.ref \
    unit_tests/ref-output/state_matrix_6.ref \
    unit_tests/ref-output/interaction_1.ref \
    unit_tests/ref-output/xml_1.ref \
    unit_tests/ref-output/xml_2.ref \
    unit_tests/ref-output/xml_3.ref \
    unit_tests/ref-output/xml_4.ref \
    unit_tests/ref-output/sm_qcd_1.ref \
    unit_tests/ref-output/sm_physics_1.ref \
    unit_tests/ref-output/sm_physics_2.ref \
    unit_tests/ref-output/models_1.ref \
    unit_tests/ref-output/models_2.ref \
    unit_tests/ref-output/models_3.ref \
    unit_tests/ref-output/models_4.ref \
    unit_tests/ref-output/models_5.ref \
    unit_tests/ref-output/models_6.ref \
    unit_tests/ref-output/models_7.ref \
    unit_tests/ref-output/auto_components_1.ref \
    unit_tests/ref-output/auto_components_2.ref \
    unit_tests/ref-output/auto_components_3.ref \
    unit_tests/ref-output/radiation_generator_1.ref \
    unit_tests/ref-output/radiation_generator_2.ref \
    unit_tests/ref-output/particles_1.ref \
    unit_tests/ref-output/particles_2.ref \
    unit_tests/ref-output/particles_3.ref \
    unit_tests/ref-output/particles_4.ref \
    unit_tests/ref-output/particles_5.ref \
    unit_tests/ref-output/particles_6.ref \
    unit_tests/ref-output/particles_7.ref \
    unit_tests/ref-output/particles_8.ref \
    unit_tests/ref-output/particles_9.ref \
    unit_tests/ref-output/beam_structures_1.ref \
    unit_tests/ref-output/beam_structures_2.ref \
    unit_tests/ref-output/beam_structures_3.ref \
    unit_tests/ref-output/beam_structures_4.ref \
    unit_tests/ref-output/beam_structures_5.ref \
    unit_tests/ref-output/beam_structures_6.ref \
    unit_tests/ref-output/sf_aux_1.ref \
    unit_tests/ref-output/sf_aux_2.ref \
    unit_tests/ref-output/sf_aux_3.ref \
    unit_tests/ref-output/sf_mappings_1.ref \
    unit_tests/ref-output/sf_mappings_2.ref \
    unit_tests/ref-output/sf_mappings_3.ref \
    unit_tests/ref-output/sf_mappings_4.ref \
    unit_tests/ref-output/sf_mappings_5.ref \
    unit_tests/ref-output/sf_mappings_6.ref \
    unit_tests/ref-output/sf_mappings_7.ref \
    unit_tests/ref-output/sf_mappings_8.ref \
    unit_tests/ref-output/sf_mappings_9.ref \
    unit_tests/ref-output/sf_mappings_10.ref \
    unit_tests/ref-output/sf_mappings_11.ref \
    unit_tests/ref-output/sf_mappings_12.ref \
    unit_tests/ref-output/sf_mappings_13.ref \
    unit_tests/ref-output/sf_mappings_14.ref \
    unit_tests/ref-output/sf_base_1.ref \
    unit_tests/ref-output/sf_base_2.ref \
    unit_tests/ref-output/sf_base_3.ref \
    unit_tests/ref-output/sf_base_4.ref \
    unit_tests/ref-output/sf_base_5.ref \
    unit_tests/ref-output/sf_base_6.ref \
    unit_tests/ref-output/sf_base_7.ref \
    unit_tests/ref-output/sf_base_8.ref \
    unit_tests/ref-output/sf_base_9.ref \
    unit_tests/ref-output/sf_base_10.ref \
    unit_tests/ref-output/sf_base_11.ref \
    unit_tests/ref-output/sf_base_12.ref \
    unit_tests/ref-output/sf_base_13.ref \
    unit_tests/ref-output/sf_base_14.ref \
    unit_tests/ref-output/sf_pdf_builtin_1.ref \
    unit_tests/ref-output/sf_pdf_builtin_2.ref \
    unit_tests/ref-output/sf_pdf_builtin_3.ref \
    unit_tests/ref-output/sf_lhapdf5_1.ref \
    unit_tests/ref-output/sf_lhapdf5_2.ref \
    unit_tests/ref-output/sf_lhapdf5_3.ref \
    unit_tests/ref-output/sf_lhapdf6_1.ref \
    unit_tests/ref-output/sf_lhapdf6_2.ref \
    unit_tests/ref-output/sf_lhapdf6_3.ref \
    unit_tests/ref-output/sf_isr_1.ref \
    unit_tests/ref-output/sf_isr_2.ref \
    unit_tests/ref-output/sf_isr_3.ref \
    unit_tests/ref-output/sf_isr_4.ref \
    unit_tests/ref-output/sf_isr_5.ref \
    unit_tests/ref-output/sf_epa_1.ref \
    unit_tests/ref-output/sf_epa_2.ref \
    unit_tests/ref-output/sf_epa_3.ref \
    unit_tests/ref-output/sf_epa_4.ref \
    unit_tests/ref-output/sf_epa_5.ref \
    unit_tests/ref-output/sf_ewa_1.ref \
    unit_tests/ref-output/sf_ewa_2.ref \
    unit_tests/ref-output/sf_ewa_3.ref \
    unit_tests/ref-output/sf_ewa_4.ref \
    unit_tests/ref-output/sf_ewa_5.ref \
    unit_tests/ref-output/sf_circe1_1.ref \
    unit_tests/ref-output/sf_circe1_2.ref \
    unit_tests/ref-output/sf_circe1_3.ref \
    unit_tests/ref-output/sf_circe2_1.ref \
    unit_tests/ref-output/sf_circe2_2.ref \
    unit_tests/ref-output/sf_circe2_3.ref \
    unit_tests/ref-output/sf_gaussian_1.ref \
    unit_tests/ref-output/sf_gaussian_2.ref \
    unit_tests/ref-output/sf_beam_events_1.ref \
    unit_tests/ref-output/sf_beam_events_2.ref \
    unit_tests/ref-output/sf_beam_events_3.ref \
    unit_tests/ref-output/sf_escan_1.ref \
    unit_tests/ref-output/sf_escan_2.ref \
    unit_tests/ref-output/phs_base_1.ref \
    unit_tests/ref-output/phs_base_2.ref \
    unit_tests/ref-output/phs_base_3.ref \
    unit_tests/ref-output/phs_base_4.ref \
    unit_tests/ref-output/phs_base_5.ref \
    unit_tests/ref-output/phs_single_1.ref \
    unit_tests/ref-output/phs_single_2.ref \
    unit_tests/ref-output/phs_single_3.ref \
    unit_tests/ref-output/phs_single_4.ref \
    unit_tests/ref-output/phs_forest_1.ref \
    unit_tests/ref-output/phs_wood_1.ref \
    unit_tests/ref-output/phs_wood_2.ref \
    unit_tests/ref-output/phs_wood_3.ref \
    unit_tests/ref-output/phs_wood_4.ref \
    unit_tests/ref-output/phs_wood_5.ref \
    unit_tests/ref-output/phs_wood_6.ref \
    unit_tests/ref-output/phs_wood_vis_1.ref \
    unit_tests/ref-output/phs_fks_generator_1.ref \
    unit_tests/ref-output/phs_fks_generator_2.ref \
    unit_tests/ref-output/phs_fks_generator_3.ref \
    unit_tests/ref-output/phs_fks_generator_4.ref \
    unit_tests/ref-output/phs_fks_generator_5.ref \
    unit_tests/ref-output/phs_fks_generator_6.ref \
    unit_tests/ref-output/fks_regions_1.ref \
    unit_tests/ref-output/fks_regions_2.ref \
    unit_tests/ref-output/fks_regions_3.ref \
    unit_tests/ref-output/nlo_color_data_1.ref \
    unit_tests/ref-output/nlo_color_data_2.ref \
    unit_tests/ref-output/nlo_color_data_3.ref \
    unit_tests/ref-output/rng_base_1.ref \
    unit_tests/ref-output/rng_base_2.ref \
    unit_tests/ref-output/rng_tao_1.ref \
    unit_tests/ref-output/rng_tao_2.ref \
    unit_tests/ref-output/selectors_1.ref \
    unit_tests/ref-output/mci_base_1.ref \
    unit_tests/ref-output/mci_base_2.ref \
    unit_tests/ref-output/mci_base_3.ref \
    unit_tests/ref-output/mci_base_4.ref \
    unit_tests/ref-output/mci_base_5.ref \
    unit_tests/ref-output/mci_base_6.ref \
    unit_tests/ref-output/mci_base_7.ref \
    unit_tests/ref-output/mci_base_8.ref \
    unit_tests/ref-output/mci_midpoint_1.ref \
    unit_tests/ref-output/mci_midpoint_2.ref \
    unit_tests/ref-output/mci_midpoint_3.ref \
    unit_tests/ref-output/mci_midpoint_4.ref \
    unit_tests/ref-output/mci_midpoint_5.ref \
    unit_tests/ref-output/mci_midpoint_6.ref \
    unit_tests/ref-output/mci_midpoint_7.ref \
    unit_tests/ref-output/mci_vamp_1.ref \
    unit_tests/ref-output/mci_vamp_2.ref \
    unit_tests/ref-output/mci_vamp_3.ref \
    unit_tests/ref-output/mci_vamp_4.ref \
    unit_tests/ref-output/mci_vamp_5.ref \
    unit_tests/ref-output/mci_vamp_6.ref \
    unit_tests/ref-output/mci_vamp_7.ref \
    unit_tests/ref-output/mci_vamp_8.ref \
    unit_tests/ref-output/mci_vamp_9.ref \
    unit_tests/ref-output/mci_vamp_10.ref \
    unit_tests/ref-output/mci_vamp_11.ref \
    unit_tests/ref-output/mci_vamp_12.ref \
    unit_tests/ref-output/mci_vamp_13.ref \
    unit_tests/ref-output/mci_vamp_14.ref \
    unit_tests/ref-output/mci_vamp_15.ref \
    unit_tests/ref-output/mci_vamp_16.ref \
    unit_tests/ref-output/prclib_interfaces_1.ref \
    unit_tests/ref-output/prclib_interfaces_2.ref \
    unit_tests/ref-output/prclib_interfaces_3.ref \
    unit_tests/ref-output/prclib_interfaces_4.ref \
    unit_tests/ref-output/prclib_interfaces_5.ref \
    unit_tests/ref-output/prclib_interfaces_6.ref \
    unit_tests/ref-output/prclib_interfaces_7.ref \
    unit_tests/ref-output/particle_specifiers_1.ref \
    unit_tests/ref-output/particle_specifiers_2.ref \
    unit_tests/ref-output/process_libraries_1.ref \
    unit_tests/ref-output/process_libraries_2.ref \
    unit_tests/ref-output/process_libraries_3.ref \
    unit_tests/ref-output/process_libraries_4.ref \
    unit_tests/ref-output/process_libraries_5.ref \
    unit_tests/ref-output/process_libraries_6.ref \
    unit_tests/ref-output/process_libraries_7.ref \
    unit_tests/ref-output/process_libraries_8.ref \
    unit_tests/ref-output/prclib_stacks_1.ref \
    unit_tests/ref-output/prclib_stacks_2.ref \
    unit_tests/ref-output/slha_1.ref \
    unit_tests/ref-output/slha_2.ref \
    unit_tests/ref-output/prc_test_1.ref \
    unit_tests/ref-output/prc_test_2.ref \
    unit_tests/ref-output/prc_test_3.ref \
    unit_tests/ref-output/prc_test_4.ref \
    unit_tests/ref-output/prc_template_me_1.ref \
    unit_tests/ref-output/prc_template_me_2.ref \
    unit_tests/ref-output/prc_omega_1.ref \
    unit_tests/ref-output/prc_omega_2.ref \
    unit_tests/ref-output/prc_omega_3.ref \
    unit_tests/ref-output/prc_omega_4.ref \
    unit_tests/ref-output/prc_omega_5.ref \
    unit_tests/ref-output/prc_omega_diags_1.ref \
    unit_tests/ref-output/parton_states_1.ref \
    unit_tests/ref-output/subevt_expr_1.ref \
    unit_tests/ref-output/subevt_expr_2.ref \
    unit_tests/ref-output/processes_1.ref \
    unit_tests/ref-output/processes_2.ref \
    unit_tests/ref-output/processes_3.ref \
    unit_tests/ref-output/processes_4.ref \
    unit_tests/ref-output/processes_5.ref \
    unit_tests/ref-output/processes_6.ref \
    unit_tests/ref-output/processes_7.ref \
    unit_tests/ref-output/processes_8.ref \
    unit_tests/ref-output/processes_9.ref \
    unit_tests/ref-output/processes_10.ref \
    unit_tests/ref-output/processes_11.ref \
    unit_tests/ref-output/processes_12.ref \
    unit_tests/ref-output/processes_13.ref \
    unit_tests/ref-output/processes_14.ref \
    unit_tests/ref-output/processes_15.ref \
    unit_tests/ref-output/processes_16.ref \
    unit_tests/ref-output/processes_17.ref \
    unit_tests/ref-output/process_stacks_1.ref \
    unit_tests/ref-output/process_stacks_2.ref \
    unit_tests/ref-output/process_stacks_3.ref \
    unit_tests/ref-output/process_stacks_4.ref \
    unit_tests/ref-output/cascades_1.ref \
    unit_tests/ref-output/cascades_2.ref \
    unit_tests/ref-output/event_transforms_1.ref \
    unit_tests/ref-output/decays_1.ref \
    unit_tests/ref-output/decays_2.ref \
    unit_tests/ref-output/decays_3.ref \
    unit_tests/ref-output/decays_4.ref \
    unit_tests/ref-output/decays_5.ref \
    unit_tests/ref-output/decays_6.ref \
    unit_tests/ref-output/shower_1.ref \
    unit_tests/ref-output/shower_2.ref \
    unit_tests/ref-output/shower_base_1.ref \
    unit_tests/ref-output/events_1.ref \
    unit_tests/ref-output/events_2.ref \
    unit_tests/ref-output/events_3.ref \
    unit_tests/ref-output/events_4.ref \
    unit_tests/ref-output/events_5.ref \
    unit_tests/ref-output/events_6.ref \
    unit_tests/ref-output/events_7.ref \
    unit_tests/ref-output/hep_events_1.ref \
    unit_tests/ref-output/eio_data_1.ref \
    unit_tests/ref-output/eio_data_2.ref \
    unit_tests/ref-output/eio_base_1.ref \
    unit_tests/ref-output/eio_raw_1.ref \
    unit_tests/ref-output/eio_raw_2.ref \
    unit_tests/ref-output/eio_checkpoints_1.ref \
    unit_tests/ref-output/eio_lhef_1.ref \
    unit_tests/ref-output/eio_lhef_2.ref \
    unit_tests/ref-output/eio_lhef_3.ref \
    unit_tests/ref-output/eio_lhef_4.ref \
    unit_tests/ref-output/eio_lhef_5.ref \
    unit_tests/ref-output/eio_lhef_6.ref \
    unit_tests/ref-output/eio_stdhep_1.ref \
    unit_tests/ref-output/eio_stdhep_2.ref \
    unit_tests/ref-output/eio_stdhep_3.ref \
    unit_tests/ref-output/eio_stdhep_4.ref \
    unit_tests/ref-output/eio_hepmc_1.ref \
    unit_tests/ref-output/eio_hepmc_2.ref \
    unit_tests/ref-output/eio_lcio_1.ref \
    unit_tests/ref-output/eio_lcio_2.ref \
    unit_tests/ref-output/eio_ascii_1.ref \
    unit_tests/ref-output/eio_ascii_2.ref \
    unit_tests/ref-output/eio_ascii_3.ref \
    unit_tests/ref-output/eio_ascii_4.ref \
    unit_tests/ref-output/eio_ascii_5.ref \
    unit_tests/ref-output/eio_ascii_6.ref \
    unit_tests/ref-output/eio_ascii_7.ref \
    unit_tests/ref-output/eio_ascii_8.ref \
    unit_tests/ref-output/eio_ascii_9.ref \
    unit_tests/ref-output/eio_ascii_10.ref \
    unit_tests/ref-output/eio_weights_1.ref \
    unit_tests/ref-output/eio_weights_2.ref \
    unit_tests/ref-output/eio_dump_1.ref \
    unit_tests/ref-output/iterations_1.ref \
    unit_tests/ref-output/iterations_2.ref \
    unit_tests/ref-output/rt_data_1.ref \
    unit_tests/ref-output/rt_data_2.ref \
    unit_tests/ref-output/rt_data_3.ref \
    unit_tests/ref-output/rt_data_4.ref \
    unit_tests/ref-output/rt_data_5.ref \
    unit_tests/ref-output/rt_data_6.ref \
    unit_tests/ref-output/rt_data_7.ref \
    unit_tests/ref-output/rt_data_8.ref \
    unit_tests/ref-output/rt_data_9.ref \
    unit_tests/ref-output/dispatch_1.ref \
    unit_tests/ref-output/dispatch_2.ref \
    unit_tests/ref-output/dispatch_7.ref \
    unit_tests/ref-output/dispatch_8.ref \
    unit_tests/ref-output/dispatch_10.ref \
    unit_tests/ref-output/dispatch_11.ref \
    unit_tests/ref-output/dispatch_rng_1.ref \
    unit_tests/ref-output/dispatch_phs_1.ref \
    unit_tests/ref-output/dispatch_phs_2.ref \
    unit_tests/ref-output/dispatch_mci_1.ref \
    unit_tests/ref-output/dispatch_transforms_1.ref \
    unit_tests/ref-output/dispatch_transforms_2.ref \
    unit_tests/ref-output/process_configurations_1.ref \
    unit_tests/ref-output/process_configurations_2.ref \
    unit_tests/ref-output/event_streams_1.ref \
    unit_tests/ref-output/event_streams_2.ref \
    unit_tests/ref-output/event_streams_3.ref \
    unit_tests/ref-output/event_streams_4.ref \
    unit_tests/ref-output/compilations_1.ref \
    unit_tests/ref-output/compilations_2.ref \
    unit_tests/ref-output/compilations_3.ref \
    unit_tests/ref-output/compilations_static_1.ref \
    unit_tests/ref-output/compilations_static_2.ref \
    unit_tests/ref-output/integrations_1.ref \
    unit_tests/ref-output/integrations_2.ref \
    unit_tests/ref-output/integrations_3.ref \
    unit_tests/ref-output/integrations_4.ref \
    unit_tests/ref-output/integrations_5.ref \
    unit_tests/ref-output/integrations_6.ref \
    unit_tests/ref-output/integrations_7.ref \
    unit_tests/ref-output/integrations_8.ref \
    unit_tests/ref-output/integrations_9.ref \
    unit_tests/ref-output/integrations_history_1.ref \
    unit_tests/ref-output/simulations_1.ref \
    unit_tests/ref-output/simulations_2.ref \
    unit_tests/ref-output/simulations_3.ref \
    unit_tests/ref-output/simulations_4.ref \
    unit_tests/ref-output/simulations_5.ref \
    unit_tests/ref-output/simulations_6.ref \
    unit_tests/ref-output/simulations_7.ref \
    unit_tests/ref-output/simulations_8.ref \
    unit_tests/ref-output/simulations_9.ref \
    unit_tests/ref-output/simulations_10.ref \
    unit_tests/ref-output/simulations_11.ref \
    unit_tests/ref-output/simulations_12.ref \
    unit_tests/ref-output/simulations_13.ref \
    unit_tests/ref-output/commands_1.ref \
    unit_tests/ref-output/commands_2.ref \
    unit_tests/ref-output/commands_3.ref \
    unit_tests/ref-output/commands_4.ref \
    unit_tests/ref-output/commands_5.ref \
    unit_tests/ref-output/commands_6.ref \
    unit_tests/ref-output/commands_7.ref \
    unit_tests/ref-output/commands_8.ref \
    unit_tests/ref-output/commands_9.ref \
    unit_tests/ref-output/commands_10.ref \
    unit_tests/ref-output/commands_11.ref \
    unit_tests/ref-output/commands_12.ref \
    unit_tests/ref-output/commands_13.ref \
    unit_tests/ref-output/commands_14.ref \
    unit_tests/ref-output/commands_15.ref \
    unit_tests/ref-output/commands_16.ref \
    unit_tests/ref-output/commands_17.ref \
    unit_tests/ref-output/commands_18.ref \
    unit_tests/ref-output/commands_19.ref \
    unit_tests/ref-output/commands_20.ref \
    unit_tests/ref-output/commands_21.ref \
    unit_tests/ref-output/commands_22.ref \
    unit_tests/ref-output/commands_23.ref \
    unit_tests/ref-output/commands_24.ref \
    unit_tests/ref-output/commands_25.ref \
    unit_tests/ref-output/commands_26.ref \
    unit_tests/ref-output/commands_27.ref \
    unit_tests/ref-output/commands_28.ref \
    unit_tests/ref-output/commands_29.ref \
    unit_tests/ref-output/commands_30.ref \
    unit_tests/ref-output/commands_31.ref \
    unit_tests/ref-output/commands_32.ref \
    unit_tests/ref-output/commands_33.ref \
    unit_tests/ref-output/commands_34.ref \
    unit_tests/ref-output/jets_1.ref \
    unit_tests/ref-output/hepmc_interface_1.ref \
    unit_tests/ref-output/lcio_interface_1.ref \
    unit_tests/ref-output/blha_1.ref \
    unit_tests/ref-output/ttv_formfactors_1.ref \
    unit_tests/ref-output/ttv_formfactors_2.ref \
    unit_tests/ref-output/blha_2.ref \
    unit_tests/ref-output/blha_3.ref \
    functional_tests/ref-output/structure_1.ref \
    functional_tests/ref-output/structure_2.ref \
    functional_tests/ref-output/structure_3.ref \
    functional_tests/ref-output/structure_4.ref \
    functional_tests/ref-output/structure_5.ref \
    functional_tests/ref-output/structure_6.ref \
    functional_tests/ref-output/structure_7.ref \
    functional_tests/ref-output/structure_8.ref \
    functional_tests/ref-output/vars.ref \
    functional_tests/ref-output/extpar.ref \
    functional_tests/ref-output/testproc_1.ref \
    functional_tests/ref-output/testproc_2.ref \
    functional_tests/ref-output/testproc_3.ref \
    functional_tests/ref-output/testproc_4.ref \
    functional_tests/ref-output/testproc_5.ref \
    functional_tests/ref-output/testproc_6.ref \
    functional_tests/ref-output/testproc_7.ref \
    functional_tests/ref-output/testproc_8.ref \
    functional_tests/ref-output/testproc_9.ref \
    functional_tests/ref-output/testproc_10.ref \
    functional_tests/ref-output/susyhit.ref \
    functional_tests/ref-output/restrictions.ref \
    functional_tests/ref-output/process_log.ref \
    functional_tests/ref-output/static_1.ref \
    functional_tests/ref-output/static_2.ref \
    functional_tests/ref-output/libraries_1.ref \
    functional_tests/ref-output/libraries_2.ref \
    functional_tests/ref-output/libraries_4.ref \
    functional_tests/ref-output/rebuild_2.ref \
    functional_tests/ref-output/rebuild_3.ref \
    functional_tests/ref-output/rebuild_4.ref \
    functional_tests/ref-output/fatal.ref \
    functional_tests/ref-output/model_change_1.ref \
    functional_tests/ref-output/model_change_2.ref \
    functional_tests/ref-output/model_scheme_1.ref \
    functional_tests/ref-output/model_test.ref \
    functional_tests/ref-output/cuts.ref \
    functional_tests/ref-output/user_cuts.ref \
    functional_tests/ref-output/user_prc_threshold_1.ref \
    functional_tests/ref-output/qedtest_1.ref \
    functional_tests/ref-output/qedtest_2.ref \
    functional_tests/ref-output/qedtest_5.ref \
    functional_tests/ref-output/qedtest_6.ref \
    functional_tests/ref-output/qedtest_7.ref \
    functional_tests/ref-output/qedtest_8.ref \
    functional_tests/ref-output/qedtest_9.ref \
    functional_tests/ref-output/qedtest_10.ref \
    functional_tests/ref-output/qcdtest_4.ref \
    functional_tests/ref-output/qcdtest_5.ref \
    functional_tests/ref-output/qcdtest_6.ref \
    functional_tests/ref-output/beam_setup_1.ref \
    functional_tests/ref-output/beam_setup_2.ref \
    functional_tests/ref-output/beam_setup_3.ref \
    functional_tests/ref-output/beam_setup_4.ref \
    functional_tests/ref-output/observables_1.ref \
    functional_tests/ref-output/event_weights_1.ref \
    functional_tests/ref-output/event_weights_2.ref \
    functional_tests/ref-output/event_eff_1.ref \
    functional_tests/ref-output/event_dump_1.ref \
    functional_tests/ref-output/reweight_1.ref \
    functional_tests/ref-output/reweight_2.ref \
    functional_tests/ref-output/reweight_3.ref \
    functional_tests/ref-output/reweight_4.ref \
    functional_tests/ref-output/reweight_5.ref \
    functional_tests/ref-output/reweight_6.ref \
    functional_tests/ref-output/reweight_7.ref \
    functional_tests/ref-output/reweight_8.ref \
    functional_tests/ref-output/analyze_1.ref \
    functional_tests/ref-output/analyze_2.ref \
    functional_tests/ref-output/analyze_3.ref \
    functional_tests/ref-output/analyze_4.ref \
    functional_tests/ref-output/colors.ref \
    functional_tests/ref-output/colors_hgg.ref \
    functional_tests/ref-output/alphas.ref \
    functional_tests/ref-output/jets_xsec.ref \
    functional_tests/ref-output/shower_err_1.ref \
    functional_tests/ref-output/parton_shower_1.ref \
    functional_tests/ref-output/pythia6_1.ref \
    functional_tests/ref-output/pythia6_2.ref \
    functional_tests/ref-output/hadronize_1.ref \
    functional_tests/ref-output/mlm_matching_fsr.ref \
    functional_tests/ref-output/mlm_pythia6_isr.ref \
    functional_tests/ref-output/hepmc_1.ref \
    functional_tests/ref-output/hepmc_2.ref \
    functional_tests/ref-output/hepmc_3.ref \
    functional_tests/ref-output/hepmc_4.ref \
    functional_tests/ref-output/hepmc_5.ref \
    functional_tests/ref-output/hepmc_6.ref \
    functional_tests/ref-output/hepmc_7.ref \
    functional_tests/ref-output/hepmc_9.ref \
    functional_tests/ref-output/hepmc_10.ref \
    functional_tests/ref-output/lhef_1.ref \
    functional_tests/ref-output/lhef_2.ref \
    functional_tests/ref-output/lhef_3.ref \
    functional_tests/ref-output/lhef_4.ref \
    functional_tests/ref-output/lhef_5.ref \
    functional_tests/ref-output/lhef_6.ref \
    functional_tests/ref-output/lhef_9.ref \
    functional_tests/ref-output/lhef_10.ref \
    functional_tests/ref-output/lhef_11.ref \
    functional_tests/ref-output/select_1.ref \
    functional_tests/ref-output/select_2.ref \
    functional_tests/ref-output/stdhep_1.ref \
    functional_tests/ref-output/stdhep_2.ref \
    functional_tests/ref-output/stdhep_3.ref \
    functional_tests/ref-output/stdhep_4.ref \
    functional_tests/ref-output/stdhep_5.ref \
    functional_tests/ref-output/stdhep_6.ref \
    functional_tests/ref-output/lcio_1.ref \
    functional_tests/ref-output/fatal_beam_decay.ref \
    functional_tests/ref-output/smtest_1.ref \
    functional_tests/ref-output/smtest_3.ref \
    functional_tests/ref-output/smtest_4.ref \
    functional_tests/ref-output/smtest_5.ref \
    functional_tests/ref-output/smtest_6.ref \
    functional_tests/ref-output/smtest_7.ref \
    functional_tests/ref-output/smtest_9.ref \
    functional_tests/ref-output/smtest_10.ref \
    functional_tests/ref-output/smtest_11.ref \
    functional_tests/ref-output/smtest_12.ref \
    functional_tests/ref-output/smtest_13.ref \
    functional_tests/ref-output/smtest_14.ref \
    functional_tests/ref-output/smtest_15.ref \
    functional_tests/ref-output/sm_cms_1.ref \
    functional_tests/ref-output/nlo_1.ref \
    functional_tests/ref-output/nlo_2.ref \
    functional_tests/ref-output/openloops_1.ref \
    functional_tests/ref-output/openloops_2.ref \
    functional_tests/ref-output/openloops_4.ref \
    functional_tests/ref-output/nlo_decay_1.ref \
    functional_tests/ref-output/mssmtest_1.ref \
    functional_tests/ref-output/mssmtest_2.ref \
    functional_tests/ref-output/spincor_1.ref \
    functional_tests/ref-output/show_1.ref \
    functional_tests/ref-output/show_2.ref \
    functional_tests/ref-output/show_3.ref \
    functional_tests/ref-output/show_4.ref \
    functional_tests/ref-output/method_ovm_1.ref \
    functional_tests/ref-output/multi_comp_4.ref \
    functional_tests/ref-output/flvsum_1.ref \
    functional_tests/ref-output/decay_err_1.ref \
    functional_tests/ref-output/decay_err_2.ref \
    functional_tests/ref-output/polarized_1.ref \
    functional_tests/ref-output/circe1_1.ref \
    functional_tests/ref-output/circe1_2.ref \
    functional_tests/ref-output/circe1_3.ref \
    functional_tests/ref-output/circe1_6.ref \
    functional_tests/ref-output/circe1_10.ref \
    functional_tests/ref-output/circe1_errors_1.ref \
    functional_tests/ref-output/circe2_1.ref \
    functional_tests/ref-output/circe2_2.ref \
    functional_tests/ref-output/circe2_3.ref \
    functional_tests/ref-output/ewa_4.ref \
    functional_tests/ref-output/gaussian_1.ref \
    functional_tests/ref-output/gaussian_2.ref \
    functional_tests/ref-output/beam_events_1.ref \
    functional_tests/ref-output/beam_events_4.ref \
    functional_tests/ref-output/energy_scan_1.ref \
    functional_tests/ref-output/user_strfun.ref

# Reference files that depend on the numerical precision
REF_OUTPUT_FILES_DOUBLE = \
    functional_tests/ref-output-double/qedtest_3.ref \
    functional_tests/ref-output-double/qedtest_4.ref \
    functional_tests/ref-output-double/qcdtest_1.ref \
    functional_tests/ref-output-double/qcdtest_2.ref \
    functional_tests/ref-output-double/qcdtest_3.ref \
    functional_tests/ref-output-double/smtest_2.ref \
    functional_tests/ref-output-double/smtest_8.ref \
    functional_tests/ref-output-double/beam_setup_5.ref \
    functional_tests/ref-output-double/nlo_3.ref \
    functional_tests/ref-output-double/nlo_4.ref \
    functional_tests/ref-output-double/fks_res_1.ref \
    functional_tests/ref-output-double/fks_res_2.ref \
    functional_tests/ref-output-double/fks_res_3.ref \
    functional_tests/ref-output-double/openloops_3.ref \
    functional_tests/ref-output-double/powheg_1.ref \
    functional_tests/ref-output-double/defaultcuts.ref \
    functional_tests/ref-output-double/parton_shower_2.ref \
    functional_tests/ref-output-double/helicity.ref \
    functional_tests/ref-output-double/lhef_7.ref \
    functional_tests/ref-output-double/hepmc_8.ref \
    functional_tests/ref-output-double/multi_comp_1.ref \
    functional_tests/ref-output-double/multi_comp_2.ref \
    functional_tests/ref-output-double/multi_comp_3.ref \
    functional_tests/ref-output-double/pdf_builtin.ref \
    functional_tests/ref-output-double/lhapdf5.ref \
    functional_tests/ref-output-double/lhapdf6.ref \
    functional_tests/ref-output-double/ep_1.ref \
    functional_tests/ref-output-double/ep_2.ref \
    functional_tests/ref-output-double/ep_3.ref \
    functional_tests/ref-output-double/circe1_4.ref \
    functional_tests/ref-output-double/circe1_5.ref \
    functional_tests/ref-output-double/circe1_7.ref \
    functional_tests/ref-output-double/circe1_8.ref \
    functional_tests/ref-output-double/circe1_9.ref \
    functional_tests/ref-output-double/circe1_photons_1.ref \
    functional_tests/ref-output-double/circe1_photons_2.ref \
    functional_tests/ref-output-double/circe1_photons_3.ref \
    functional_tests/ref-output-double/circe1_photons_4.ref \
    functional_tests/ref-output-double/circe1_photons_5.ref \
    functional_tests/ref-output-double/pythia6_3.ref \
    functional_tests/ref-output-double/pythia6_4.ref \
    functional_tests/ref-output-double/tauola_1.ref \
    functional_tests/ref-output-double/tauola_2.ref \
    functional_tests/ref-output-double/mlm_matching_isr.ref \
    functional_tests/ref-output-double/ewa_1.ref \
    functional_tests/ref-output-double/ewa_2.ref \
    functional_tests/ref-output-double/ewa_3.ref \
    functional_tests/ref-output-double/ilc.ref \
    functional_tests/ref-output-double/user_prc_threshold_2.ref \
    functional_tests/ref-output-double/beam_events_2.ref \
    functional_tests/ref-output-double/beam_events_3.ref

REF_OUTPUT_FILES_PREC = \
    functional_tests/ref-output-prec/qedtest_3.ref \
    functional_tests/ref-output-prec/qedtest_4.ref \
    functional_tests/ref-output-prec/qcdtest_1.ref \
    functional_tests/ref-output-prec/qcdtest_2.ref \
    functional_tests/ref-output-prec/qcdtest_3.ref \
    functional_tests/ref-output-prec/smtest_2.ref \
    functional_tests/ref-output-prec/smtest_8.ref \
    functional_tests/ref-output-prec/beam_setup_5.ref \
    functional_tests/ref-output-prec/nlo_3.ref \
    functional_tests/ref-output-prec/nlo_4.ref \
    functional_tests/ref-output-prec/fks_res_1.ref \
    functional_tests/ref-output-prec/fks_res_2.ref \
    functional_tests/ref-output-prec/fks_res_3.ref \
    functional_tests/ref-output-prec/openloops_3.ref \
    functional_tests/ref-output-prec/defaultcuts.ref \
    functional_tests/ref-output-prec/parton_shower_2.ref \
    functional_tests/ref-output-prec/helicity.ref \
    functional_tests/ref-output-prec/lhef_7.ref \
    functional_tests/ref-output-prec/multi_comp_1.ref \
    functional_tests/ref-output-prec/multi_comp_2.ref \
    functional_tests/ref-output-prec/multi_comp_3.ref \
    functional_tests/ref-output-prec/pdf_builtin.ref \
    functional_tests/ref-output-prec/lhapdf5.ref \
    functional_tests/ref-output-prec/lhapdf6.ref \
    functional_tests/ref-output-prec/ep_1.ref \
    functional_tests/ref-output-prec/ep_2.ref \
    functional_tests/ref-output-prec/ep_3.ref \
    functional_tests/ref-output-prec/circe1_9.ref \
    functional_tests/ref-output-prec/circe1_photons_1.ref \
    functional_tests/ref-output-prec/circe1_photons_2.ref \
    functional_tests/ref-output-prec/circe1_photons_3.ref \
    functional_tests/ref-output-prec/circe1_photons_4.ref \
    functional_tests/ref-output-prec/circe1_photons_5.ref \
    functional_tests/ref-output-prec/user_prc_threshold_2.ref \
    functional_tests/ref-output-prec/ewa_1.ref

REF_OUTPUT_FILES_EXT = \
    functional_tests/ref-output-ext/circe1_4.ref \
    functional_tests/ref-output-ext/circe1_5.ref \
    functional_tests/ref-output-ext/circe1_7.ref \
    functional_tests/ref-output-ext/circe1_8.ref \
    functional_tests/ref-output-ext/powheg_1.ref \
    functional_tests/ref-output-ext/pythia6_3.ref \
    functional_tests/ref-output-ext/pythia6_4.ref \
    functional_tests/ref-output-ext/tauola_1.ref \
    functional_tests/ref-output-ext/tauola_2.ref \
    functional_tests/ref-output-ext/ewa_2.ref \
    functional_tests/ref-output-ext/ewa_3.ref \
    functional_tests/ref-output-ext/ilc.ref \
    functional_tests/ref-output-ext/beam_events_2.ref \
    functional_tests/ref-output-ext/beam_events_3.ref \
    functional_tests/ref-output-ext/mlm_matching_isr.ref \
    functional_tests/ref-output-ext/hepmc_8.ref

REF_OUTPUT_FILES_QUAD = \
    functional_tests/ref-output-quad/circe1_4.ref \
    functional_tests/ref-output-quad/circe1_5.ref \
    functional_tests/ref-output-quad/circe1_7.ref \
    functional_tests/ref-output-quad/circe1_8.ref \
    functional_tests/ref-output-quad/powheg_1.ref \
    functional_tests/ref-output-quad/pythia6_3.ref \
    functional_tests/ref-output-quad/pythia6_4.ref \
    functional_tests/ref-output-quad/tauola_1.ref \
    functional_tests/ref-output-quad/tauola_2.ref \
    functional_tests/ref-output-quad/ewa_2.ref \
    functional_tests/ref-output-quad/ewa_3.ref \
    functional_tests/ref-output-quad/ilc.ref \
    functional_tests/ref-output-quad/beam_events_2.ref \
    functional_tests/ref-output-quad/beam_events_3.ref \
    functional_tests/ref-output-quad/mlm_matching_isr.ref \
    functional_tests/ref-output-quad/hepmc_8.ref

TESTSUITES_M4 = \
    $(MISC_TESTS_M4) \
    $(MSSM_EXT_M4) \
    $(NMSSM_EXT_M4)

TESTSUITES_SIN = \
    $(MISC_TESTS_SIN) \
    $(MSSM_EXT_SIN) \
    $(NMSSM_EXT_SIN) \
    $(SHOWER_EXT_SIN) \
    $(NLO_EXT_SIN)

MISC_TESTS_M4 =

MISC_TESTS_SIN = \
    functional_tests/empty.sin \
    functional_tests/fatal.sin \
    functional_tests/defaultcuts.sin \
    functional_tests/cuts.sin \
    functional_tests/model_change_1.sin \
    functional_tests/model_change_2.sin \
    functional_tests/model_scheme_1.sin \
    functional_tests/model_test.sin \
    functional_tests/structure_1.sin \
    functional_tests/structure_2.sin \
    functional_tests/structure_3.sin \
    functional_tests/structure_4.sin \
    functional_tests/structure_5.sin \
    functional_tests/structure_6.sin \
    functional_tests/structure_7.sin \
    functional_tests/structure_8.sin \
    functional_tests/vars.sin \
    functional_tests/extpar.sin \
    functional_tests/testproc_1.sin \
    functional_tests/testproc_2.sin \
    functional_tests/testproc_3.sin \
    functional_tests/testproc_4.sin \
    functional_tests/testproc_5.sin \
    functional_tests/testproc_6.sin \
    functional_tests/testproc_7.sin \
    functional_tests/testproc_8.sin \
    functional_tests/testproc_9.sin \
    functional_tests/testproc_10.sin \
    functional_tests/libraries_1.sin \
    functional_tests/libraries_2.sin \
    functional_tests/libraries_3.sin \
    functional_tests/libraries_4.sin \
    functional_tests/rebuild_1.sin \
    functional_tests/rebuild_2.sin \
    functional_tests/rebuild_3.sin \
    functional_tests/rebuild_4.sin \
    functional_tests/rebuild_5.sin \
    functional_tests/qedtest_1.sin \
    functional_tests/qedtest_2.sin \
    functional_tests/qedtest_3.sin \
    functional_tests/qedtest_4.sin \
    functional_tests/qedtest_5.sin \
    functional_tests/qedtest_6.sin \
    functional_tests/qedtest_7.sin \
    functional_tests/qedtest_8.sin \
    functional_tests/qedtest_9.sin \
    functional_tests/qedtest_10.sin \
    functional_tests/beam_setup_1.sin \
    functional_tests/beam_setup_2.sin \
    functional_tests/beam_setup_3.sin \
    functional_tests/beam_setup_4.sin \
    functional_tests/beam_setup_5.sin \
    functional_tests/qcdtest_1.sin \
    functional_tests/qcdtest_2.sin \
    functional_tests/qcdtest_3.sin \
    functional_tests/qcdtest_4.sin \
    functional_tests/qcdtest_5.sin \
    functional_tests/qcdtest_6.sin \
    functional_tests/observables_1.sin \
    functional_tests/event_weights_1.sin \
    functional_tests/event_weights_2.sin \
    functional_tests/event_eff_1.sin \
    functional_tests/event_dump_1.sin \
    functional_tests/reweight_1.sin \
    functional_tests/reweight_2.sin \
    functional_tests/reweight_3.sin \
    functional_tests/reweight_4.sin \
    functional_tests/reweight_5.sin \
    functional_tests/reweight_6.sin \
    functional_tests/reweight_7.sin \
    functional_tests/reweight_8.sin \
    functional_tests/analyze_1.sin \
    functional_tests/analyze_2.sin \
    functional_tests/analyze_3.sin \
    functional_tests/analyze_4.sin \
    functional_tests/colors.sin \
    functional_tests/colors_hgg.sin \
    functional_tests/alphas.sin \
    functional_tests/jets_xsec.sin \
    functional_tests/lhef_1.sin \
    functional_tests/lhef_2.sin \
    functional_tests/lhef_3.sin \
    functional_tests/lhef_4.sin \
    functional_tests/lhef_5.sin \
    functional_tests/lhef_6.sin \
    functional_tests/lhef_7.sin \
    functional_tests/lhef_8.sin \
    functional_tests/lhef_9.sin \
    functional_tests/lhef_10.sin \
    functional_tests/lhef_11.sin \
    functional_tests/select_1.sin \
    functional_tests/select_2.sin \
    functional_tests/shower_err_1.sin \
    functional_tests/parton_shower_1.sin \
    functional_tests/parton_shower_2.sin \
    functional_tests/pythia6_1.sin \
    functional_tests/pythia6_2.sin \
    functional_tests/pythia6_3.sin \
    functional_tests/pythia6_4.sin \
    functional_tests/hadronize_1.sin \
    functional_tests/tauola_1.sin \
    functional_tests/tauola_2.sin \
    functional_tests/mlm_matching_fsr.sin \
    functional_tests/mlm_matching_isr.sin \
    functional_tests/mlm_pythia6_isr.sin \
    functional_tests/hepmc_1.sin \
    functional_tests/hepmc_2.sin \
    functional_tests/hepmc_3.sin \
    functional_tests/hepmc_4.sin \
    functional_tests/hepmc_5.sin \
    functional_tests/hepmc_6.sin \
    functional_tests/hepmc_7.sin \
    functional_tests/hepmc_8.sin \
    functional_tests/hepmc_9.sin \
    functional_tests/hepmc_10.sin \
    functional_tests/stdhep_1.sin \
    functional_tests/stdhep_2.sin \
    functional_tests/stdhep_3.sin \
    functional_tests/stdhep_4.sin \
    functional_tests/stdhep_5.sin \
    functional_tests/stdhep_6.sin \
    functional_tests/lcio_1.sin \
    functional_tests/fatal_beam_decay.sin \
    functional_tests/smtest_1.sin \
    functional_tests/smtest_2.sin \
    functional_tests/smtest_3.sin \
    functional_tests/smtest_4.sin \
    functional_tests/smtest_5.sin \
    functional_tests/smtest_6.sin \
    functional_tests/smtest_7.sin \
    functional_tests/smtest_8.sin \
    functional_tests/smtest_9.sin \
    functional_tests/smtest_10.sin \
    functional_tests/smtest_11.sin \
    functional_tests/smtest_12.sin \
    functional_tests/smtest_13.sin \
    functional_tests/smtest_14.sin \
    functional_tests/smtest_15.sin \
    functional_tests/sm_cms_1.sin \
    functional_tests/nlo_1.sin \
    functional_tests/nlo_2.sin \
    functional_tests/nlo_3.sin \
    functional_tests/nlo_4.sin \
    functional_tests/nlo_decay_1.sin \
    functional_tests/fks_res_1.sin \
    functional_tests/fks_res_2.sin \
    functional_tests/fks_res_3.sin \
    functional_tests/openloops_1.sin \
    functional_tests/openloops_2.sin \
    functional_tests/openloops_3.sin \
    functional_tests/openloops_4.sin \
    functional_tests/powheg_1.sin \
    functional_tests/mssmtest_1.sin \
    functional_tests/mssmtest_2.sin \
    functional_tests/spincor_1.sin \
    functional_tests/show_1.sin \
    functional_tests/show_2.sin \
    functional_tests/show_3.sin \
    functional_tests/show_4.sin \
    functional_tests/method_ovm_1.sin \
    functional_tests/multi_comp_1.sin \
    functional_tests/multi_comp_2.sin \
    functional_tests/multi_comp_3.sin \
    functional_tests/multi_comp_4.sin \
    functional_tests/flvsum_1.sin \
    functional_tests/decay_err_1.sin \
    functional_tests/decay_err_2.sin \
    functional_tests/polarized_1.sin \
    functional_tests/pdf_builtin.sin \
    functional_tests/lhapdf5.sin \
    functional_tests/lhapdf6.sin \
    functional_tests/ep_1.sin \
    functional_tests/ep_2.sin \
    functional_tests/ep_3.sin \
    functional_tests/circe1_1.sin \
    functional_tests/circe1_2.sin \
    functional_tests/circe1_3.sin \
    functional_tests/circe1_4.sin \
    functional_tests/circe1_5.sin \
    functional_tests/circe1_6.sin \
    functional_tests/circe1_7.sin \
    functional_tests/circe1_8.sin \
    functional_tests/circe1_9.sin \
    functional_tests/circe1_10.sin \
    functional_tests/circe1_photons_1.sin \
    functional_tests/circe1_photons_2.sin \
    functional_tests/circe1_photons_3.sin \
    functional_tests/circe1_photons_4.sin \
    functional_tests/circe1_photons_5.sin \
    functional_tests/circe1_errors_1.sin \
    functional_tests/circe2_1.sin \
    functional_tests/circe2_2.sin \
    functional_tests/circe2_3.sin \
    functional_tests/ewa_1.sin \
    functional_tests/ewa_2.sin \
    functional_tests/ewa_3.sin \
    functional_tests/ewa_4.sin \
    functional_tests/ilc.sin \
    functional_tests/gaussian_1.sin \
    functional_tests/gaussian_2.sin \
    functional_tests/beam_events_1.sin \
    functional_tests/beam_events_2.sin \
    functional_tests/beam_events_3.sin \
    functional_tests/beam_events_4.sin \
    functional_tests/energy_scan_1.sin \
    functional_tests/susyhit.sin \
    functional_tests/restrictions.sin \
    functional_tests/helicity.sin \
    functional_tests/process_log.sin \
    functional_tests/static_1.sin \
    functional_tests/static_1.exe.sin \
    functional_tests/static_2.sin \
    functional_tests/static_2.exe.sin \
    functional_tests/user_cuts.sin \
    functional_tests/user_prc_threshold_1.sin \
    functional_tests/user_prc_threshold_2.sin \
    functional_tests/user_strfun.sin \
    ilc_ext.sin

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

SHOWER_EXT_SIN = \
    extra_shower_1.sin \
    extra_shower_2.sin \
    extra_shower_3_bb.sin \
    extra_shower_3_jj.sin \
    extra_shower_3_qqqq.sin \
    extra_shower_3_tt.sin \
    extra_shower_3_z_nu.sin \
    extra_shower_3_z_tau.sin \
    extra_shower_4_ee.sin \
    extra_shower_5.sin \
    extra_shower_6.sin

NLO_EXT_SIN = \
    extra_nlo_decay.sin \
    extra_nlo_tt.sin \
    extra_nlo_tt_powheg.sin \
    extra_nlo_tt_powheg_sudakov.sin \
    extra_nlo_uu.sin \
    extra_nlo_uu_powheg.sin \
    extra_nlo_threshold.sin \
    extra_threshold_factorized.sin \
    extra_nlo_methods_gosam.sin

all-local: $(TESTSUITES_SIN)

if M4_AVAILABLE
SUFFIXES = .m4 .sin
.m4.sin: 
	$(M4) $(srcdir)/$(TESTSUITE_MACROS) $< > $@
endif M4_AVAILABLE

