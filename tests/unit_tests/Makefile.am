## Makefile.am -- Makefile for executable WHIZARD test scripts
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
#     Felix Braam, Sebastian Schmidt
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

WHIZARD_UT_DRIVER = run_whizard_ut.sh

UNIT_TESTS = \
    analysis.run \
    pdg_arrays.run \
    expressions.run \
    beams.run \
    su_algebra.run \
    bloch_vectors.run \
    polarizations.run \
    md5.run \
    cputime.run \
    lexers.run \
    parser.run \
    color.run \
    os_interface.run \
    evaluators.run \
    formats.run \
    sorting.run \
    grids.run \
    solver.run \
    state_matrices.run \
    interactions.run \
    xml.run \
    sm_qcd.run \
    sm_physics.run \
    models.run \
    auto_components.run \
    radiation_generator.run \
    blha.run \
    particles.run \
    beam_structures.run \
    sf_aux.run \
    sf_mappings.run \
    sf_base.run \
    sf_pdf_builtin.run \
    sf_isr.run \
    sf_epa.run \
    sf_ewa.run \
    sf_circe1.run \
    sf_circe2.run \
    sf_gaussian.run \
    sf_beam_events.run \
    sf_escan.run \
    phs_base.run \
    phs_single.run \
    phs_forests.run \
    phs_wood.run \
    phs_fks_generator.run \
    fks_regions.run \
    nlo_color_data.run \
    rng_base.run \
    rng_tao.run \
    selectors.run \
    mci_base.run \
    mci_midpoint.run \
    mci_vamp.run \
    prclib_interfaces.run \
    particle_specifiers.run \
    process_libraries.run \
    prclib_stacks.run \
    slha_interface.run \
    prc_test.run \
    prc_template_me.run \
    parton_states.run \
    subevt_expr.run \
    processes.run \
    process_stacks.run \
    cascades.run \
    event_transforms.run \
    decays.run \
    shower.run \
    shower_base.run \
    events.run \
    hep_events.run \
    eio_data.run \
    eio_base.run \
    eio_raw.run \
    eio_checkpoints.run \
    eio_lhef.run \
    eio_stdhep.run \
    eio_ascii.run \
    eio_weights.run \
    eio_dump.run \
    iterations.run \
    rt_data.run \
    dispatch.run \
    dispatch_rng.run \
    dispatch_mci.run \
    dispatch_phs.run \
    dispatch_transforms.run \
    process_configurations.run \
    event_streams.run \
    integrations.run \
    ttv_formfactors.run \
    simulations.run

XFAIL_UNIT_TESTS =

UNIT_TESTS_REQ_GAMELAN = \
    commands.run

UNIT_TESTS_REQ_EV_ANA = \
    phs_wood_vis.run \
    prc_omega_diags.run \
    integrations_history.run

UNIT_TESTS_REQ_FASTJET = \
    jets.run

UNIT_TESTS_REQ_HEPMC = \
    hepmc.run \
    eio_hepmc.run

UNIT_TESTS_REQ_LCIO = \
    lcio.run \
    eio_lcio.run

UNIT_TESTS_REQ_OCAML = \
    prc_omega.run \
    compilations.run \
    compilations_static.run

UNIT_TESTS_REQ_LHAPDF5 = \
    sf_lhapdf5.run 
UNIT_TESTS_REQ_LHAPDF6 = \
    sf_lhapdf6.run 

TEST_DRIVERS_RUN = \
    $(UNIT_TESTS) \
    $(UNIT_TESTS_REQ_GAMELAN) \
    $(UNIT_TESTS_REQ_HEPMC) \
    $(UNIT_TESTS_REQ_LCIO) \
    $(UNIT_TESTS_REQ_FASTJET) \
    $(UNIT_TESTS_REQ_LHAPDF5) \
    $(UNIT_TESTS_REQ_LHAPDF6) \
    $(UNIT_TESTS_REQ_OCAML)

TEST_DRIVERS_SH = $(TEST_DRIVERS_RUN:.run=.sh)

########################################################################

TESTS = 
XFAIL_TESTS =
TESTS_SRC =

UNIT_TESTS += $(UNIT_TESTS_REQ_GAMELAN)
UNIT_TESTS += $(UNIT_TESTS_REQ_FASTJET)
UNIT_TESTS += $(UNIT_TESTS_REQ_HEPMC)
UNIT_TESTS += $(UNIT_TESTS_REQ_LCIO)
UNIT_TESTS += $(UNIT_TESTS_REQ_LHAPDF5)
UNIT_TESTS += $(UNIT_TESTS_REQ_LHAPDF6)
UNIT_TESTS += $(UNIT_TESTS_REQ_OCAML)
UNIT_TESTS += $(UNIT_TESTS_REQ_EV_ANA)

TESTS += $(UNIT_TESTS)
XFAIL_TESTS += $(XFAIL_UNIT_TESTS)


EXTRA_DIST = $(TEST_DRIVERS_SH)
    $(TESTS_SRC)

########################################################################
# Force building the whizard_ut executable in the main src directory.
# This depends on the unit-test libraries which will be built recursively.

WHIZARD_UT = ../../src/whizard_ut

$(TEST_DRIVERS_RUN): $(WHIZARD_UT)

$(WHIZARD_UT):
	$(MAKE) -C ../../src check

########################################################################

VPATH = $(srcdir)

SUFFIXES = .sh .run

.sh.run:
	@rm -f $@
	@cp $< $@
	@chmod +x $@

sf_beam_events.run: test_beam_events.dat
test_beam_events.dat: $(top_builddir)/share/beam-sim/test_beam_events.dat
	cp $< $@

commands.run: sps1ap_decays.slha
sps1ap_decays.slha: $(top_builddir)/share/susy/sps1ap_decays.slha
	cp $< $@

WT_OMEGA_CACHE_SUFFIX=vertices
WT_OCAML_NATIVE_EXT=opt

if MPOST_AVAILABLE
$(UNIT_TESTS_REQ_GAMELAN): gamelan.sty
gamelan.sty: $(top_builddir)/src/gamelan/gamelan.sty
	cp $< $@

$(top_builddir)/src/gamelan/gamelan.sty:
	$(MAKE) -C $(top_builddir)/src/gamelan gamelan.sty
endif

BUILT_SOURCES = \
    TESTFLAG  \
    HEPMC_FLAG \
    LCIO_FLAG \
    FASTJET_FLAG \
    LHAPDF5_FLAG \
    LHAPDF6_FLAG \
    GAMELAN_FLAG \
    EVENT_ANALYSIS_FLAG \
    OCAML_FLAG \
    PYTHIA6_FLAG \
    STATIC_FLAG \
    ref-output \
    err-output

# If this file is found in the working directory, WHIZARD
# will use the paths for the uninstalled version (source/build tree),
# otherwise it uses the installed version
TESTFLAG:
	touch $@

FASTJET_FLAG:
if FASTJET_AVAILABLE
	touch $@
endif

HEPMC_FLAG:
if HEPMC_AVAILABLE
	touch $@
endif

LCIO_FLAG:
if LCIO_AVAILABLE
	touch $@
endif

LHAPDF5_FLAG:
if LHAPDF5_AVAILABLE
	touch $@
endif

LHAPDF6_FLAG:
if LHAPDF6_AVAILABLE
	touch $@
endif

GAMELAN_FLAG:
if MPOST_AVAILABLE
	touch $@
endif

OCAML_FLAG:
if OCAML_AVAILABLE
	touch $@
endif

PYTHIA6_FLAG:
if PYTHIA6_AVAILABLE
	touch $@
endif

EVENT_ANALYSIS_FLAG:
if EVENT_ANALYSIS_AVAILABLE
	touch $@
endif

STATIC_FLAG:
if STATIC_AVAILABLE
	touch $@
endif

# The reference output files are in the source directory.  Copy them here.
ref-output: $(top_srcdir)/share/tests/unit_tests/ref-output
	mkdir -p ref-output
	for f in $</*.ref; do cp $$f $@; done

# The output files from self tests go here.
err-output:
	mkdir -p err-output

## installcheck runs the test scripts with the TESTFLAG removed.
## NOTE: disabled for unit tests, because whizard_ut will not be installed.
installcheck-local:
#installcheck-local: notestflag check-am
notestflag:
	rm -f TESTFLAG
.PHONY: notestflag

## Remove generated files
clean-local:
	rm -f gamelan.sty
	rm -f TESTFLAG GAMELAN_FLAG 
	rm -f OCAML_FLAG FASTJET_FLAG HEPMC_FLAG LCIO_FLAG 
	rm -f EVENT_ANALYSIS_FLAG PYTHIA6_FLAG LHAPDF5_FLAG
	rm -f LHAPDF6_FLAG STATIC_FLAG static_1.exe
	rm -f *.run *.log slha_test.out
	rm -f core* 
	rm -f *.f90 *.c *.$(FC_MODULE_EXT) *.o *.la
	rm -f *.makefile
	rm -f *.grid
	rm -rf err-output
	rm -rf ref-output
	rm -f *.sin *.hbc 
	rm -f *.phs *.vg *.vgb *.evt *.evx *.lhe *.hepmc *.dat *.debug *.mdl
	rm -f *.tmp *.hepevt *.hepevt.verb *.lha *.lha.verb *.slcio
	rm -f prc_omega_diags_1_p_i1_diags.out prc_omega_diags_1_p_i1_diags.toc
	rm -f *.hep *.up.hep *.[1-9] *.[1-9][0-9] *.[1-9][0-9][0-9] 
	rm -f *.tex *.mp *.mpx *.t[1-9] *.t[1-9][0-9] *.t[1-9][0-9][0-9] 
	rm -f *.ltp *.aux *.dvi *.ps *.pdf so_test.*
	rm -f *.tbl sps1ap_decays.slha bar structure_6[a-b].out 
	rm -f *.$(WT_OMEGA_CACHE_SUFFIX)
if FC_SUBMODULES
	rm -f *.smod
endif

## Remove backup files
maintainer-clean-local: maintainer-clean-fc
	-rm -f *~
.PHONY: maintainer-clean-local
