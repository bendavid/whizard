## Makefile.am -- Makefile for executable WHIZARD test scripts
##
## Process this file with automake to produce Makefile.in
##
########################################################################
#
# Copyright (C) 1999-2015 by 
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

WHIZARD_DRIVER = run_whizard.sh

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
    rebuild_4.run \
    susyhit.run \
    helicity.run \
    libraries_4.run

XFAIL_TESTS_DEFAULT =

TESTS_USER_STRFUN = \
    user_strfun.run 

TESTS_REQ_FASTJET = \
    analyze_4.run

TESTS_REQ_OCAML = \
    libraries_1.run \
    libraries_2.run \
    libraries_3.run \
    rebuild_2.run \
    rebuild_3.run \
    rebuild_5.run \
    defaultcuts.run \
    cuts.run \
    model_change_1.run \
    model_change_2.run \
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
    beam_setup_5.run \
    qcdtest_1.run \
    qcdtest_2.run \
    qcdtest_3.run \
    qcdtest_4.run \
    qcdtest_5.run \
    observables_1.run \
    event_weights_1.run \
    event_weights_2.run \
    event_eff_1.run \
    event_dump_1.run \
    reweight_1.run \
    reweight_2.run \
    reweight_3.run \
    reweight_4.run \
    reweight_5.run \
    reweight_6.run \
    reweight_7.run \
    reweight_8.run \
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
    lhef_7.run \
    lhef_8.run \
    lhef_9.run \
    lhef_10.run \
    lhef_11.run \
    stdhep_1.run \
    stdhep_2.run \
    stdhep_3.run \
    stdhep_4.run \
    stdhep_5.run \
    stdhep_6.run \
    select_1.run \
    select_2.run \
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
    nlo_1.run \
    nlo_2.run \
    nlo_3.run \
    nlo_4.run \
    powheg_1.run \
    spincor_1.run \
    show_1.run \
    show_2.run \
    show_3.run \
    show_4.run \
    method_ovm_1.run \
    multi_comp_1.run \
    multi_comp_2.run \
    multi_comp_3.run \
    multi_comp_4.run \
    flvsum_1.run \
    decay_err_1.run \
    decay_err_2.run \
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
    circe1_10.run \
    circe1_photons_1.run \
    circe1_photons_2.run \
    circe1_photons_3.run \
    circe1_photons_4.run \
    circe1_photons_5.run \
    circe1_errors_1.run \
    circe2_1.run \
    circe2_2.run \
    circe2_3.run \
    ewa_1.run \
    ewa_2.run \
    ewa_3.run \
    ewa_4.run \
    ilc.run \
    gaussian_1.run \
    gaussian_2.run \
    beam_events_1.run \
    beam_events_2.run \
    beam_events_3.run \
    beam_events_4.run \
    energy_scan_1.run \
    restrictions.run \
    process_log.run \
    shower_err_1.run \
    parton_shower_1.run \
    parton_shower_2.run \
    mlm_matching_fsr.run \
    user_cuts.run \
    user_prc_threshold.run \
    $(TESTS_USER_STRFUN) 

XFAIL_TESTS_REQ_OCAML = \
    colors_hgg.run \
    user_cuts.run \
    $(TESTS_USER_STRFUN)

TESTS_REQ_HEPMC = \
    hepmc_1.run \
    hepmc_2.run \
    hepmc_3.run \
    hepmc_4.run \
    hepmc_5.run \
    hepmc_6.run \
    hepmc_7.run \
    hepmc_8.run \
    hepmc_9.run \
    hepmc_10.run
XFAIL_TESTS_REQ_HEPMC =

TESTS_REQ_LHAPDF5 = \
    lhapdf5.run
TESTS_REQ_LHAPDF6 = \
    lhapdf6.run
XFAIL_TESTS_REQ_LHAPDF5 =
XFAIL_TESTS_REQ_LHAPDF6 =

TESTS_STATIC = \
    static_1.run \
    static_2.run
XFAIL_TESTS_STATIC =

TESTS_REQ_PYTHIA6 = \
    pythia6_1.run \
    pythia6_2.run \
    pythia6_3.run \
    pythia6_4.run \
    mlm_matching_isr.run \
    mlm_pythia6_isr.run
XFAIL_TESTS_REQ_PYTHIA6 = 

TESTS_REQ_EV_ANA = \
    analyze_3.run
XFAIL_TESTS_REQ_EV_ANA =

TESTS_REQ_GAMELAN = \
    analyze_3.run

TEST_DRIVERS_RUN = \
    $(TESTS_DEFAULT) \
    $(TESTS_REQ_OCAML) \
    $(TESTS_REQ_LHAPDF5) \
    $(TESTS_REQ_LHAPDF6) \
    $(TESTS_REQ_HEPMC) \
    $(TESTS_REQ_FASTJET) \
    $(TESTS_REQ_PYTHIA6) \
    $(TESTS_REQ_EV_ANA) \
    $(TESTS_STATIC)

TEST_DRIVERS_SH = $(TEST_DRIVERS_RUN:.run=.sh)

########################################################################

TESTS = 
XFAIL_TESTS =
TESTS_SRC =

TESTS += $(TESTS_DEFAULT)
XFAIL_TESTS += $(XFAIL_TESTS_DEFAULT)

TESTS += $(TESTS_REQ_OCAML)
XFAIL_TESTS += $(XFAIL_TESTS_REQ_OCAML)

TESTS += $(TESTS_REQ_HEPMC)
XFAIL_TESTS += $(XFAIL_TESTS_REQ_HEPMC)

TESTS += $(TESTS_REQ_FASTJET)
XFAIL_TESTS += $(XFAIL_TESTS_REQ_FASTJET)

TESTS += $(TESTS_REQ_LHAPDF5)
XFAIL_TESTS += $(XFAIL_TESTS_REQ_LHAPDF5)

TESTS += $(TESTS_REQ_LHAPDF6)
XFAIL_TESTS += $(XFAIL_TESTS_REQ_LHAPDF6)

TESTS += $(TESTS_REQ_PYTHIA6)
XFAIL_TESTS += $(XFAIL_TESTS_REQ_PYTHIA6)

TESTS += $(TESTS_REQ_EV_ANA)
XFAIL_TESTS += $(XFAIL_TESTS_REQ_EV_ANA)

TESTS += $(TESTS_STATIC)
XFAIL_TESTS += $(XFAIL_TESTS_STATIC)


EXTRA_DIST = $(TEST_DRIVERS_SH) \
    $(TESTS_SRC)

########################################################################

VPATH = $(srcdir)

SUFFIXES = .sh .run 

.sh.run:
	@rm -f $@
	@if test -f $(top_builddir)/share/tests/functional_tests/$*.sin; then \
	  $(SED) 's|@script@|$(top_builddir)/share/tests/functional_tests/$*|g' $< > $@; \
	elif test -f $(top_srcdir)/share/tests/functional_tests/$*.sin; then \
	  $(SED) 's|@script@|$(top_srcdir)/share/tests/functional_tests/$*|g' $< > $@; \
	else \
	  echo "$*.sin not found!" 1>&2; \
	  exit 2; \
	fi
	@chmod +x $@

structure_2.run: structure_2_inc.sin
structure_2_inc.sin: $(top_builddir)/share/tests/functional_tests/structure_2_inc.sin
	cp $< $@

testproc_3.run: testproc_3.phs
testproc_3.phs: $(top_builddir)/share/tests/functional_tests/testproc_3.phs
	cp $< $@

static_1.run: static_1.exe.sin
static_1.exe.sin: $(top_builddir)/share/tests/functional_tests/static_1.exe.sin
	cp $< $@
static_2.run: static_2.exe.sin
static_2.exe.sin: $(top_builddir)/share/tests/functional_tests/static_2.exe.sin
	cp $< $@

susyhit.run: susyhit.in
user_cuts.run: user_cuts.f90
user_strfun.run: user_strfun.f90
user_cuts.f90: $(top_builddir)/share/tests/functional_tests/user_cuts.f90
	cp $< $@
user_strfun.f90: $(top_builddir)/share/tests/functional_tests/user_strfun.f90
	cp $< $@

model_test.run: tdefs.$(FC_MODULE_EXT) tglue.$(FC_MODULE_EXT) \
	threeshl.$(FC_MODULE_EXT) tscript.$(FC_MODULE_EXT)
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
OMEGA_QED = $(top_builddir)/omega/bin/omega_QED.$(WT_OCAML_NATIVE_EXT)
omega_QED.$(WT_OMEGA_CACHE_SUFFIX): $(OMEGA_QED)
	$(OMEGA_QED) -initialize .

qcdtest_1.run qcdtest_2.run: omega_QCD.$(WT_OMEGA_CACHE_SUFFIX)
OMEGA_QCD = $(top_builddir)/omega/bin/omega_QCD.$(WT_OCAML_NATIVE_EXT)
omega_QCD.$(WT_OMEGA_CACHE_SUFFIX): $(OMEGA_QCD)
	$(OMEGA_QCD) -initialize .
endif OCAML_AVAILABLE

if MPOST_AVAILABLE
$(TESTS_REQ_GAMELAN): gamelan.sty
gamelan.sty: $(top_builddir)/src/gamelan/gamelan.sty
	cp $< $@

$(top_builddir)/src/gamelan/gamelan.sty:
	$(MAKE) -C $(top_builddir)/src/gamelan gamelan.sty
endif

if HEPMC_AVAILABLE
TESTS_SRC += $(hepmc_6_rd_SOURCES)
noinst_PROGRAMS = hepmc_6_rd
hepmc_6_rd_SOURCES = hepmc_6_rd.cpp
hepmc_6_rd_CXXFLAGS = $(HEPMC_INCLUDES) $(AM_CXXFLAGS)
hepmc_6_rd_LDADD = $(LDFLAGS_HEPMC)
hepmc_6.run: hepmc_6_rd
endif

stdhep_4.run: stdhep_rd
stdhep_5.run: stdhep_rd
stdhep_6.run: stdhep_rd
stdhep_rd: $(top_builddir)/src/xdr/stdhep_rd
	cp $< $@

susyhit.in: $(top_builddir)/share/tests/functional_tests/susyhit.in
	cp $< $@

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
    ref-output

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
if FC_QUAD
ref-output: $(top_srcdir)/share/tests/functional_tests/ref-output
	mkdir -p ref-output
	for f in $</*.ref; do cp $$f $@; done
	for f in $</../ref-output-prec/*.ref; do cp $$f $@; done
	for f in $</../ref-output-quad/*.ref; do cp $$f $@; done
else
if FC_EXT
ref-output: $(top_srcdir)/share/tests/functional_tests/ref-output
	mkdir -p ref-output
	for f in $</*.ref; do cp $$f $@; done
	for f in $</../ref-output-prec/*.ref; do cp $$f $@; done
	for f in $</../ref-output-ext/*.ref; do cp $$f $@; done
else
ref-output: $(top_srcdir)/share/tests/functional_tests/ref-output
	mkdir -p ref-output
	for f in $</*.ref; do cp $$f $@; done
	for f in $</../ref-output-double/*.ref; do cp $$f $@; done
endif
endif

## installcheck runs the test scripts with the TESTFLAG removed.
installcheck-local: notestflag check-am
notestflag:
	rm -f TESTFLAG
.PHONY: notestflag

## Remove generated files
clean-local:
	rm -f gamelan.sty TEST.mdl 
	rm -f TESTFLAG GAMELAN_FLAG 
	rm -f OCAML_FLAG FASTJET_FLAG HEPMC_FLAG LCIO_FLAG 
	rm -f EVENT_ANALYSIS_FLAG PYTHIA6_FLAG LHAPDF5_FLAG
	rm -f LHAPDF6_FLAG STATIC_FLAG static_1.exe static_2.exe
	rm -f *.run *.log slha_test.out
	rm -f core* stdhep_rd
	rm -f *.f90 *.f90.in *.c *.$(FC_MODULE_EXT) *.o *.la
	rm -f *.makefile
	rm -f *.grid
	rm -rf err-output
	rm -rf ref-output
	rm -f *.sin *.hbc 
	rm -f *.phs *.vg *.vgb *.evt *.evx *.lhe *.hepmc *.dat *.debug 
	rm -f *.tmp *.hepevt *.hepevt.verb *.lha *.lha.verb *.slcio
	rm -f prc_omega_diags_1_p_i1_diags.out prc_omega_diags_1_p_i1_diags.toc
	rm -f *.hep *.up.hep *.[1-9] *.[1-9][0-9] *.[1-9][0-9][0-9] 
	rm -f *.tex *.mp *.mpx *.t[1-9] *.t[1-9][0-9] *.t[1-9][0-9][0-9] 
	rm -f *.ltp *.aux *.dvi *.ps *.pdf so_test.*
	rm -f *.tbl sps1ap_decays.slha bar structure_6[a-b].out 
	rm -f slhaspectrum.in suspect2.out suspect2_lha.out 
	rm -f susyhit.in susyhit_slha.out suspect2_lha.in
	rm -f *.$(WT_OMEGA_CACHE_SUFFIX)
if FC_SUBMODULES
	rm -f *.smod
endif

## Remove backup files
maintainer-clean-local: maintainer-clean-fc
	-rm -f *~
.PHONY: maintainer-clean-local



