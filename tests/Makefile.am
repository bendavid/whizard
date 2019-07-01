## Makefile.am -- Makefile for executable WHIZARD test scripts
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

WHIZARD_DRIVER = run_whizard.sh

UNIT_TESTS = \
    analysis.run \
    expressions.run \
    beams.run \
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
    state_matrices.run \
    interactions.run \
    xml.run \
    sm_qcd.run \
    models.run \
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
    sf_beam_events.run \
    sf_escan.run \
    phs_base.run \
    phs_single.run \
    phs_forests.run \
    phs_wood.run \
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
    subevt_expr.run \
    processes.run \
    process_stacks.run \
    cascades.run \
    event_transforms.run \
    decays.run \
    shower.run \
    events.run \
    eio_data.run \
    eio_base.run \
    eio_raw.run \
    eio_checkpoints.run \
    eio_lhef.run \
    eio_ascii.run \
    eio_weights.run \
    iterations.run \
    rt_data.run \
    dispatch.run \
    process_configurations.run \
    event_streams.run \
    integrations.run \
    simulations.run

UNIT_TESTS_REQ_GAMELAN = \
    commands.run

UNIT_TESTS_REQ_EV_ANA = \
    phs_wood_vis.run \
    prc_omega_diags.run \
    integrations_history.run

UNIT_TESTS_REQ_HEPMC = \
    eio_hepmc.run

UNIT_TESTS_REQ_STDHEP = \
    eio_stdhep.run

UNIT_TESTS_REQ_OCAML = \
    prc_omega.run \
    compilations.run \
    compilations_static.run

TESTS_DEFAULT = \
    empty.run \
    fatal.run \
    structure_1.run \
    structure_2.run \
    structure_3.run \
    structure_4.run \
    structure_5.run \
    structure_6.run \
    structure_7.run \
    structure_8.run \
    vars.run \
    extpar.run \
    testproc_1.run \
    testproc_2.run \
    testproc_3.run \
    testproc_4.run \
    testproc_5.run \
    testproc_6.run \
    testproc_7.run \
    testproc_8.run \
    testproc_9.run \
    testproc_10.run \
    rebuild_1.run \
    susyhit.run \
    helicity.run \
    libraries_4.run

XFAIL_TESTS_DEFAULT =

TESTS_USER_STRFUN = \
    user_strfun.run 

TESTS_REQ_OCAML = \
    libraries_1.run \
    libraries_2.run \
    libraries_3.run \
    rebuild_2.run \
    defaultcuts.run \
    cuts.run \
    model_test.run \
    qedtest_1.run \
    qedtest_2.run \
    qedtest_3.run \
    qedtest_4.run \
    qedtest_5.run \
    qedtest_6.run \
    qedtest_7.run \
    qedtest_8.run \
    qedtest_9.run \
    qedtest_10.run \
    beam_setup_1.run \
    beam_setup_2.run \
    beam_setup_3.run \
    beam_setup_4.run \
    qcdtest_1.run \
    qcdtest_2.run \
    qcdtest_3.run \
    qcdtest_4.run \
    qcdtest_5.run \
    observables_1.run \
    event_weights_1.run \
    event_weights_2.run \
    reweight_1.run \
    reweight_2.run \
    reweight_3.run \
    reweight_4.run \
    reweight_5.run \
    reweight_6.run \
    reweight_7.run \
    analyze_1.run \
    analyze_2.run \
    colors.run \
    colors_hgg.run \
    alphas.run \
    jets_xsec.run \
    lhef_1.run \
    lhef_2.run \
    lhef_3.run \
    lhef_4.run \
    lhef_5.run \
    lhef_6.run \
    fatal_beam_decay.run \
    smtest_1.run \
    smtest_2.run \
    smtest_3.run \
    smtest_4.run \
    smtest_5.run \
    smtest_6.run \
    smtest_7.run \
    smtest_8.run \
    smtest_9.run \
    smtest_10.run \
    smtest_11.run \
    smtest_12.run \
    smtest_13.run \
    smtest_14.run \
    smtest_15.run \
    mssmtest.run \
    show_1.run \
    show_2.run \
    show_3.run \
    show_4.run \
    multi_comp_1.run \
    multi_comp_2.run \
    multi_comp_3.run \
    multi_comp_4.run \
    flvsum_1.run \
    pdf_builtin.run \
    ep_1.run \
    ep_2.run \
    ep_3.run \
    circe1_1.run \
    circe1_2.run \
    circe1_3.run \
    circe1_4.run \
    circe1_5.run \
    circe1_6.run \
    circe1_7.run \
    circe1_8.run \
    circe1_9.run \
    circe1_photons_1.run \
    circe1_photons_2.run \
    circe1_photons_3.run \
    circe1_photons_4.run \
    circe1_photons_5.run \
    circe1_errors_1.run \
    circe2_1.run \
    circe2_2.run \
    ewa_1.run \
    ewa_2.run \
    ewa_3.run \
    ewa_4.run \
    ilc.run \
    beam_events_1.run \
    beam_events_2.run \
    beam_events_3.run \
    energy_scan_1.run \
    restrictions.run \
    process_log.run \
    user_cuts.run \
    $(TESTS_USER_STRFUN) 
XFAIL_TESTS_REQ_OCAML = \
    colors_hgg.run \
    user_cuts.run \
    $(TESTS_USER_STRFUN)

TESTS_REQ_HEPMC = \
    hepmc.run \
    hepmc_1.run \
    hepmc_2.run \
    hepmc_3.run \
    hepmc_4.run

UNIT_TESTS_REQ_LHAPDF = \
    sf_lhapdf.run 

TESTS_REQ_LHAPDF = \
    lhapdf.run
XFAIL_TESTS_REQ_LHAPDF =

TESTS_REQ_STDHEP = \
    stdhep_1.run \
    stdhep_2.run \
    stdhep_3.run
XFAIL_TESTS_REQ_STDHEP =

TESTS_STATIC = \
    static_1.run
XFAIL_TESTS_STATIC =

TESTS_REQ_SHOWER = \
    parton_shower_1.run \
    parton_shower_2.run \
    parton_shower_3.run \
    parton_shower_4.run \
    mlm_matching_fsr.run \
    mlm_matching_isr.run	
XFAIL_TESTS_REQ_SHOWER = 

TESTS_REQ_EV_ANA = \
    analyze_3.run
XFAIL_TESTS_REQ_EV_ANA =

TESTS_EXTENDED = \
    sm_ext.run \
    mssm_ext.run \
    nmssm_ext.run
XFAIL_TESTS_EXTENDED =

TESTS_REQ_GAMELAN = #\
    $(TESTS_REQ_OCAML) \
    $(TESTS_EXTENDED)

TEST_DRIVERS_RUN = \
    $(UNIT_TESTS) \
    $(UNIT_TESTS_REQ_GAMELAN) \
    $(UNIT_TESTS_REQ_HEPMC) \
    $(UNIT_TESTS_REQ_LHAPDF) \
    $(UNIT_TESTS_REQ_STDHEP) \
    $(UNIT_TESTS_REQ_OCAML) \
    $(TESTS_DEFAULT) \
    $(TESTS_REQ_OCAML) \
    $(TESTS_REQ_LHAPDF) \
    $(TESTS_REQ_HEPMC) \
    $(TESTS_REQ_STDHEP) \
    $(TESTS_REQ_SHOWER) \
    $(TESTS_REQ_EV_ANA) \
    $(TESTS_STATIC) \
    $(TESTS_EXTENDED)

TEST_DRIVERS_SH = $(TEST_DRIVERS_RUN:.run=.sh)

########################################################################

TESTS = 
XFAIL_TESTS =

UNIT_TESTS += $(UNIT_TESTS_REQ_GAMELAN)
UNIT_TESTS += $(UNIT_TESTS_REQ_HEPMC)
UNIT_TESTS += $(UNIT_TESTS_REQ_LHAPDF)
UNIT_TESTS += $(UNIT_TESTS_REQ_STDHEP)
UNIT_TESTS += $(UNIT_TESTS_REQ_OCAML)
UNIT_TESTS += $(UNIT_TESTS_REQ_EV_ANA)

# Temporarily, only the new unit tests are executed
if DEFAULT_CHECKS

TESTS += $(UNIT_TESTS)
TESTS += $(TESTS_DEFAULT)
XFAIL_TESTS += $(XFAIL_TESTS_DEFAULT)

TESTS += $(TESTS_REQ_OCAML)
XFAIL_TESTS += $(XFAIL_TESTS_REQ_OCAML)

TESTS += $(TESTS_REQ_HEPMC)
XFAIL_TESTS += $(XFAIL_TESTS_REQ_HEPMC)

TESTS += $(TESTS_REQ_LHAPDF)
XFAIL_TESTS += $(XFAIL_TESTS_REQ_LHAPDF)

TESTS += $(TESTS_REQ_STDHEP)
XFAIL_TESTS += $(XFAIL_TESTS_REQ_STDHEP)

TESTS += $(TESTS_REQ_SHOWER)
XFAIL_TESTS += $(XFAIL_TESTS_REQ_SHOWER)

TESTS += $(TESTS_REQ_EV_ANA)
XFAIL_TESTS += $(XFAIL_TESTS_REQ_EV_ANA)

TESTS += $(TESTS_STATIC)
XFAIL_TESTS += $(XFAIL_TESTS_STATIC)

endif DEFAULT_CHECKS

if EXT_NUM_CHECKS
TESTS += $(TESTS_EXTENDED)
XFAIL_TESTS += $(XFAIL_TESTS_EXTENDED)
endif EXT_NUM_CHECKS

mssm_ext.run: $(EXTENDED_MSSM_TEST_DRIVERS)
	echo '#! /bin/sh' >$@
	for driver in $(EXTENDED_MSSM_TEST_DRIVERS); do \
	  echo ./$$driver; \
	done >>$@
	chmod +x $@

nmssm_ext.run: $(EXTENDED_NMSSM_TEST_DRIVERS)
	echo '#! /bin/sh' >$@
	for driver in $(EXTENDED_NMSSM_TEST_DRIVERS); do \
	  echo ./$$driver; \
	done >>$@
	chmod +x $@

EXTENDED_MSSM_TEST_DRIVERS = \
    mssm_ext-ee.run mssm_ext-ee2.run \
    mssm_ext-en.run mssm_ext-tn.run \
    mssm_ext-uu.run mssm_ext-uu2.run mssm_ext-uuckm.run \
    mssm_ext-dd.run mssm_ext-dd2.run mssm_ext-ddckm.run \
    mssm_ext-bb.run mssm_ext-bt.run mssm_ext-tt.run \
    mssm_ext-ug.run mssm_ext-dg.run \
    mssm_ext-aa.run mssm_ext-wa.run mssm_ext-za.run \
    mssm_ext-ww.run mssm_ext-wz.run mssm_ext-zz.run \
    mssm_ext-gg.run mssm_ext-ga.run mssm_ext-gw.run mssm_ext-gz.run

EXTENDED_NMSSM_TEST_DRIVERS = \
    nmssm_ext-aa.run  nmssm_ext-bb1.run nmssm_ext-bb2.run \
    nmssm_ext-bt.run  nmssm_ext-dd1.run \
    nmssm_ext-dd2.run nmssm_ext-ee1.run \
    nmssm_ext-ee2.run nmssm_ext-en.run  \
    nmssm_ext-ga.run  nmssm_ext-gg.run  \
    nmssm_ext-gw.run  nmssm_ext-gz.run  \
    nmssm_ext-qg.run  nmssm_ext-tn.run  \
    nmssm_ext-tt1.run nmssm_ext-tt2.run \
    nmssm_ext-uu1.run nmssm_ext-uu2.run \
    nmssm_ext-wa.run  nmssm_ext-ww1.run nmssm_ext-ww2.run \
    nmssm_ext-wz.run  nmssm_ext-za.run

EXTENDED_MSSM_TEST_DRIVERS_SH = $(EXTENDED_MSSM_TEST_DRIVERS:.run=.sh)
EXTENDED_NMSSM_TEST_DRIVERS_SH = $(EXTENDED_NMSSM_TEST_DRIVERS:.run=.sh)

EXTRA_DIST = $(TEST_DRIVERS_SH) \
    $(EXTENDED_MSSM_TEST_DRIVERS_SH) \
    $(EXTENDED_NMSSM_TEST_DRIVERS_SH)

########################################################################

# VPATH = $(top_builddir)/share/tests:$(top_srcdir)/share/tests:$(srcdir)
VPATH = $(srcdir)

SUFFIXES = .sh .run

.sh.run:
	@rm -f $@
	@if test -f $(top_builddir)/share/tests/$*.sin; then \
	  $(SED) 's|@script@|$(top_builddir)/share/tests/$*|g' $< > $@; \
	elif test -f $(top_srcdir)/share/tests/$*.sin; then \
	  $(SED) 's|@script@|$(top_srcdir)/share/tests/$*|g' $< > $@; \
	else \
	  echo "$*.sin not found!" 1>&2; \
	  exit 2; \
        fi
	chmod +x $@

structure_2.run: structure_2_inc.sin
structure_2_inc.sin: $(top_builddir)/share/tests/structure_2_inc.sin
	cp $< $@

testproc_3.run: testproc_3.phs
testproc_3.phs: $(top_builddir)/share/tests/testproc_3.phs
	cp $< $@

static_1.run: static_1.exe.sin
static_1.exe.sin: $(top_builddir)/share/tests/static_1.exe.sin
	cp $< $@

sf_beam_events.run: test_beam_events.dat
test_beam_events.dat: $(top_builddir)/share/beam-sim/test_beam_events.dat
	cp $< $@

commands.run: sps1ap_decays.slha
sps1ap_decays.slha: $(top_builddir)/share/susy/sps1ap_decays.slha
	cp $< $@

susyhit.run: susyhit.in
user_cuts.run: user_cuts.f90
user_strfun.run: user_strfun.f90
user_cuts.f90: $(top_builddir)/share/tests/user_cuts.f90
	cp $< $@
user_strfun.f90: $(top_builddir)/share/tests/user_strfun.f90
	cp $< $@

model_test.run: tdefs.mod tglue.mod threeshl.mod tscript.mod
tdefs.mod: $(top_builddir)/src/models/threeshl_bundle/tdefs.$(FC_MODULE_EXT)
	cp $< $@
tglue.mod: $(top_builddir)/src/models/threeshl_bundle/tglue.$(FC_MODULE_EXT)
	cp $< $@
tscript.mod: $(top_builddir)/src/models/threeshl_bundle/tscript.$(FC_MODULE_EXT)
	cp $< $@
threeshl.mod: $(top_builddir)/src/models/threeshl_bundle/threeshl.$(FC_MODULE_EXT)
	cp $< $@

TEST.mdl: $(top_srcdir)/share/tests/TEST.mdl
	cp $< $@

WT_OMEGA_CACHE_SUFFIX=vertices
WT_OCAML_NATIVE_EXT=opt

if OCAML_AVAILABLE
qedtest_1.run qedtest_2.run qedtest_3.run qedtest_4.run qedtest_5.run qedtest_6.run: omega_QED.$(WT_OMEGA_CACHE_SUFFIX)
OMEGA_QED = $(top_builddir)/src/omega/bin/omega_QED.$(WT_OCAML_NATIVE_EXT)
omega_QED.$(WT_OMEGA_CACHE_SUFFIX): $(OMEGA_QED)
	$(OMEGA_QED) -initialize .

qcdtest_1.run qcdtest_2.run: omega_QCD.$(WT_OMEGA_CACHE_SUFFIX)
OMEGA_QCD = $(top_builddir)/src/omega/bin/omega_QCD.$(WT_OCAML_NATIVE_EXT)
omega_QCD.$(WT_OMEGA_CACHE_SUFFIX): $(OMEGA_QCD)
	$(OMEGA_QCD) -initialize .
endif OCAML_AVAILABLE

if MPOST_AVAILABLE
$(TESTS_REQ_GAMELAN): gamelan.sty
$(UNIT_TESTS_REQ_GAMELAN): gamelan.sty
gamelan.sty: $(top_builddir)/src/gamelan/gamelan.sty
	cp $< $@

$(top_builddir)/src/gamelan/gamelan.sty:
	$(MAKE) -C $(top_builddir)/src/gamelan gamelan.sty
endif

susyhit.in: $(top_builddir)/share/tests/susyhit.in
	cp $< $@

BUILT_SOURCES = \
    TESTFLAG  \
    STDHEP_FLAG \
    HEPMC_FLAG \
    LHAPDF_FLAG \
    GAMELAN_FLAG \
    EVENT_ANALYSIS_FLAG \
    OCAML_FLAG \
    SHOWER_FLAG \
    STATIC_FLAG \
    ref-output \
    err-output

# If this file is found in the working directory, WHIZARD
# will use the paths for the uninstalled version (source/build tree),
# otherwise it uses the installed version
TESTFLAG:
	touch $@

STDHEP_FLAG:
if STDHEP_AVAILABLE
	touch $@
endif

HEPMC_FLAG:
if HEPMC_AVAILABLE
	touch $@
endif

LHAPDF_FLAG:
if LHAPDF_AVAILABLE
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

SHOWER_FLAG:
if SHOWER_AVAILABLE
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
if FC_QUAD
ref-output: $(top_srcdir)/share/tests/ref-output 
	mkdir -p ref-output
	for f in $</*.ref; do cp $$f $@; done
	for f in $</../ref-output-quad/*.ref; do cp $$f $@; done
else
ref-output: $(top_srcdir)/share/tests/ref-output 
	mkdir -p ref-output
	for f in $</*.ref; do cp $$f $@; done
	for f in $</../ref-output-double/*.ref; do cp $$f $@; done
endif

# The output files from self tests go here.
err-output:
	mkdir -p err-output

## installcheck runs the test scripts with the TESTFLAG removed.
installcheck-local: notestflag check-am
notestflag:
	rm -f TESTFLAG
.PHONY: notestflag

## Remove generated files
clean-local:
	rm -f gamelan.sty TEST.mdl 
	rm -f TESTFLAG GAMELAN_FLAG STDHEP_FLAG 
	rm -f OCAML_FLAG HEPMC_FLAG LHAPDF_FLAG
	rm -f EVENT_ANALYSIS_FLAG SHOWER_FLAG
	rm -f STATIC_FLAG static_1.exe
	rm -f *.run *.log slha_test.out
	rm -f *.f90 *.c *.$(FC_MODULE_EXT) *.o *.la
	rm -f *.makefile
	rm -rf err-output
	rm -rf ref-output
	rm -f *.sin
	rm -f *.phs *.vg *.vgb *.evt *.evx *.lhe *.hepmc *.dat *.debug 
	rm -f *.tmp *.hepevt *.hepevt.verb *.lha *.lha.verb
	rm -f prc_omega_diags_1_p_i1_diags.out prc_omega_diags_1_p_i1_diags.toc
	rm -f *.stdhep *.up.stdhep *.[1-9] *.[1-9][0-9] *.[1-9][0-9][0-9] 
	rm -f *.tex *.mp *.mpx *.t[1-9] *.t[1-9][0-9] *.t[1-9][0-9][0-9] 
	rm -f *.ltp *.aux *.dvi *.ps *.pdf so_test.*
	rm -f *.tbl sps1ap_decays.slha bar structure_6[a-b].out 
	rm -f slhaspectrum.in suspect2.out suspect2_lha.out 
	rm -f susyhit.in susyhit_slha.out suspect2_lha.in
	rm -f *.$(WT_OMEGA_CACHE_SUFFIX)

## Remove backup files
maintainer-clean-local: maintainer-clean-fc
	-rm -f *~
.PHONY: maintainer-clean-local



