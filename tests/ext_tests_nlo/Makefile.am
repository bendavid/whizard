## Makefile.am -- Makefile for executable WHIZARD test scripts
##
## Process this file with automake to produce Makefile.in
##
########################################################################
#
# Copyright (C) 1999-2019 by
#     Wolfgang Kilian <kilian@physik.uni-siegen.de>
#     Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
#     Juergen Reuter <juergen.reuter@desy.de>
#     with contributions from
#     cf. main AUTHORS file
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

TESTS_EXTENDED = \
	nlo_decay_tbw.run \
	nlo_ee4b.run \
	nlo_ee4j.run \
	nlo_ee4t.run \
	nlo_ee4tj.run \
	nlo_ee5j.run \
	nlo_eebb.run \
	nlo_eebbj.run \
	nlo_eebbjj.run \
	nlo_eejj.run \
	nlo_eejjj.run \
	nlo_eett.run \
	nlo_eetta.run \
	nlo_eettaa.run \
	nlo_eettah.run \
	nlo_eettaj.run \
	nlo_eettajj.run \
	nlo_eettaz.run \
	nlo_eettbb.run \
	nlo_eetth.run \
	nlo_eetthh.run \
	nlo_eetthj.run \
	nlo_eetthjj.run \
	nlo_eetthz.run \
	nlo_eettj.run \
	nlo_eettjj.run \
	nlo_eettjjj.run \
	nlo_eettwjj.run \
	nlo_eettww.run \
	nlo_eettz.run \
	nlo_eettzj.run \
	nlo_eettzjj.run \
	nlo_eettzz.run \
	nlo_fks_delta_o_eejj.run \
	nlo_fks_delta_i_ppee.run \
	nlo_jets.run \
	nlo_methods_gosam.run \
	nlo_qq_powheg.run \
	nlo_threshold_factorized.run \
	nlo_threshold.run \
	nlo_tt_powheg_sudakov.run \
	nlo_tt_powheg.run \
	nlo_tt.run \
	nlo_uu_powheg.run \
	nlo_uu.run

XFAIL_TESTS_EXTENDED = \
	nlo_ee4b.run \
	nlo_ee4j.run \
	nlo_ee4tj.run \
	nlo_ee5j.run \
	nlo_eebbj.run \
	nlo_eebbjj.run \
	nlo_eejjj.run \
	nlo_eettaj.run \
	nlo_eettajj.run \
	nlo_eettbb.run \
	nlo_eetthjj.run \
	nlo_eettjj.run \
	nlo_eettjjj.run \
	nlo_eettzjj.run

TESTS_REQ_GAMELAN = \
	nlo_tt_powheg.run \
	nlo_uu_powheg.run

TEST_DRIVERS_RUN = \
    $(TESTS_EXTENDED)

TEST_DRIVERS_SH = $(TEST_DRIVERS_RUN:.run=.sh)

########################################################################

TESTS = $(TESTS_EXTENDED)
XFAIL_TESTS = $(XFAIL_TESTS_EXTENDED)

EXTRA_DIST = $(TEST_DRIVERS_SH)

########################################################################

VPATH = $(srcdir)

SUFFIXES = .sh .run

.sh.run:
	@rm -f $@
	@if test -f $(top_builddir)/share/tests/ext_tests_nlo/$*.sin; then \
	  $(SED) 's|@script@|$(top_builddir)/share/tests/ext_tests_nlo/$*|g' $< > $@; \
	elif test -f $(top_srcdir)/share/tests/ext_tests_nlo/$*.sin; then \
	  $(SED) 's|@script@|$(top_srcdir)/share/tests/ext_tests_nlo/$*|g' $< > $@; \
	fi
	$(SED) 's|@python_bin@|$(PYTHON_BIN)|g' $@ >> "$@.tmp" && mv "$@.tmp" $@
	$(SED) 's|@share_dir@|$(top_srcdir)/share/tests|g' $@ >> "$@.tmp" && mv "$@.tmp" $@
	@chmod +x $@

nlo_eejj.run: nlo_settings.sin
nlo_eejjj.run: nlo_settings.sin
nlo_ee4j.run: nlo_settings.sin
nlo_ee5j.run: nlo_settings.sin
nlo_eebb.run: nlo_settings.sin
nlo_eebbj.run: nlo_settings.sin
nlo_eebbjj.run: nlo_settings.sin
nlo_ee4b.run: nlo_settings.sin
nlo_eett.run: nlo_settings.sin
nlo_eettj.run: nlo_settings.sin
nlo_eettjj.run: nlo_settings.sin
nlo_eettjjj.run: nlo_settings.sin
nlo_eettbb.run: nlo_settings.sin
nlo_eetta.run: nlo_settings.sin
nlo_eettaa.run: nlo_settings.sin
nlo_eettaj.run: nlo_settings.sin
nlo_eettajj.run: nlo_settings.sin
nlo_eettah.run: nlo_settings.sin
nlo_eettaz.run: nlo_settings.sin
nlo_eettz.run: nlo_settings.sin
nlo_eettzj.run: nlo_settings.sin
nlo_eettzjj.run: nlo_settings.sin
nlo_eettzz.run: nlo_settings.sin
nlo_eettwjj.run: nlo_settings.sin
nlo_eettww.run: nlo_settings.sin
nlo_eetth.run: nlo_settings.sin
nlo_eetthj.run: nlo_settings.sin
nlo_eetthjj.run: nlo_settings.sin
nlo_eetthh.run: nlo_settings.sin
nlo_eetthz.run: nlo_settings.sin
nlo_ee4t.run: nlo_settings.sin
nlo_ee4tj.run: nlo_settings.sin
nlo_settings.sin: $(top_builddir)/share/tests/ext_tests_nlo/nlo_settings.sin
	cp $< $@

if MPOST_AVAILABLE
$(TESTS_REQ_GAMELAN): gamelan.sty
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
    PYTHON_FLAG \
    PYTHIA6_FLAG \
    OPENLOOPS_FLAG \
    GOSAM_FLAG \
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

PYTHON_FLAG:
if PYTHON_AVAILABLE
	touch $@
endif

PYTHIA6_FLAG:
if PYTHIA6_AVAILABLE
	touch $@
endif

OPENLOOPS_FLAG:
if OPENLOOPS_AVAILABLE
	touch $@
endif

GOSAM_FLAG:
if GOSAM_AVAILABLE
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
ref-output: $(top_srcdir)/share/tests/ext_tests_nlo/ref-output
	mkdir -p ref-output
	for f in $</*.ref; do cp $$f $@; done

## installcheck runs the test scripts with the TESTFLAG removed.
installcheck-local: notestflag check-am
notestflag:
	rm -f TESTFLAG
.PHONY: notestflag

## Remove generated files
clean-local:
	rm -f gamelan.sty
	rm -f TESTFLAG
	rm -f *_FLAG
	rm -f LHAPDF6_FLAG STATIC_FLAG static_1.exe
	rm -f *.run *.log slha_test.out
	rm -f core* stdhep_rd
	rm -f *.f90 *.c *.$(FC_MODULE_EXT) *.o *.la *.f90.in
	rm -f *.makefile
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
	rm -f *.olc *.olp *.olp_parameters
	rm -rf stability_log
	rm -rf *olp_modules
	rm -rf Generated_Loops
	rm -rf include lib golem.in
	rm -f SM_tt_threshold.grid
	rm -f *fks_regions.out
	rm -f *_powheg_*.pg
if FC_SUBMODULES
	rm -f *.smod
endif

## Remove backup files
maintainer-clean-local: maintainer-clean-fc
	-rm -f *~
.PHONY: maintainer-clean-local
