## Makefile.am -- Makefile for executable WHIZARD test scripts
##
## Process this file with automake to produce Makefile.in
##
########################################################################
#
# Copyright (C) 1999-2010 by 
#     Wolfgang Kilian <kilian@hep.physik.uni-siegen.de>
#     Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
#     Juergen Reuter <juergen.reuter@physik.uni-freiburg.de>
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
    vars.run \
    md5.run \
    errors.run \
    extpar.run \
    susyhit.run \
    test_me.run
XFAIL_TESTS_DEFAULT = \
    errors.run

TESTS_REQ_OCAML = \
    libs.run \
    qedtest.run \
    helicity.run \
    smtest.run \
    defaultcuts.run \
    restrictions.run \
    decays.run \
    alphas.run \
    colors.run \
    cuts.run \
    ilc.run \
    mssmtest.run \
    models.run
XFAIL_TESTS_REQ_OCAML =

TESTS_REQ_LHAPDF = \
    lhapdf.run
XFAIL_TESTS_REQ_LHAPDF =

TESTS_REQ_STDHEP = \
    stdhep.run \
    stdhep_up.run
XFAIL_TESTS_REQ_STDHEP =

TESTS_STATC = \
    static.run
XFAIL_TESTS_STATIC =

TESTS_EXTENDED = \
    sm_ext.run \
    mssm_ext.run \
    nmssm_ext.run
XFAIL_TESTS_EXTENDED =

TEST_DRIVERS_RUN = \
    $(TESTS_DEFAULT) \
    $(TESTS_REQ_OCAML) \
    $(TESTS_REQ_LHAPDF) \
    $(TESTS_REQ_STDHEP) \
    $(TESTS_STATIC) \
    $(TESTS_EXTENDED)

TEST_DRIVERS_SH = $(TEST_DRIVERS_RUN:.run=.sh)

EXTRA_DIST = $(TEST_DRIVERS_SH)

########################################################################

TESTS = 
XFAIL_TESTS =

if DEFAULT_CHECKS

TESTS += $(TESTS_DEFAULT)
XFAIL_TESTS += $(XFAIL_TESTS_DEFAULT)

if OCAML_AVAILABLE 

TESTS += $(TESTS_REQ_OCAML)
XFAIL_TESTS += $(XFAIL_TESTS_REQ_OCAML)

if LHAPDF_AVAILABLE
TESTS += $(TESTS_REQ_LHAPDF)
XFAIL_TESTS += $(XFAIL_TESTS_REQ_LHAPDF)
endif LHAPDF_AVAILABLE

if STDHEP_AVAILABLE
TESTS += $(TESTS_REQ_STDHEP)
XFAIL_TESTS += $(XFAIL_TESTS_REQ_STDHEP)
endif STDHEP_AVAILABLE

if STATIC_AVAILABLE
TESTS += $(TESTS_STATIC)
XFAIL_TESTS += $(XFAIL_TESTS_STATIC)
endif STATIC_AVAILABLE

if EXT_NUM_CHECKS
TESTS += $(TESTS_EXTENDED)
XFAIL_TESTS += $(XFAIL_TESTS_EXTENDED)
endif EXT_NUM_CHECKS

endif OCAML_AVAILABLE 
endif DEFAULT_CHECKS

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

# VPATH = $(top_builddir)/share/test:$(top_srcdir)/share/test:$(srcdir)
VPATH = $(srcdir)

SUFFIXES = .sh .run

.sh.run:
	@rm -f $@
	@if test -f $(top_builddir)/share/test/$*.sin; then \
	  $(SED) 's|@script@|$(top_builddir)/share/test/$*|g' $< > $@; \
	elif test -f $(top_srcdir)/share/test/$*.sin; then \
	  $(SED) 's|@script@|$(top_srcdir)/share/test/$*|g' $< > $@; \
	else \
	  echo "$*.sin not found!" 1>&2; \
	  exit 2; \
        fi
	chmod +x $@

plain.run: TEST.mdl
qedtest.run: gamelan.sty
susyhit.run: susyhit.in

TEST.mdl: $(top_srcdir)/share/test/TEST.mdl
	cp $< $@

gamelan.sty: $(top_builddir)/src/gamelan/gamelan.sty
	cp $< $@

if MPOST_AVAILABLE
$(top_builddir)/src/gamelan/gamelan.sty:
	$(MAKE) -C $(top_builddir)/src/gamelan gamelan.sty
endif

susyhit.in: $(top_builddir)/share/test/susyhit.in
	cp $< $@

BUILT_SOURCES = \
    TESTFLAG

# If this file is found in the working directory, WHIZARD
# will use the paths for the uninstalled version (source/build tree),
# otherwise it uses the installed version
TESTFLAG:
	touch $@

## installcheck runs the test scripts with the TESTFLAG removed.
installcheck-local: notestflag check-am
notestflag:
	rm -f TESTFLAG
.PHONY: notestflag

## Remove generated files
clean-local:
	rm -f gamelan.sty TESTFLAG TEST.mdl
	rm -f whizard.static
	rm -f *.run *.log
	rm -f *.f90 *.mod *.o *.la
	rm -f *.phs *.vg *.evt *.evx *.lhef *.hepmc *.hepevt
	rm -f *.tmp *.dat *.debug *.lha *.short *.long *.athena
	rm -f *.stdhep *.stdhep_up
	rm -f *.tex *.mp *.mpx *.[1-9] *.[1-9][0-9] *.[1-9][0-9][0-9] 
	rm -f *.ltp *.aux *.dvi *.ps *.pdf
	rm -f slhaspectrum.in suspect2.out suspect2_lha.out 
	rm -f susyhit.in susyhit_slha.out suspect2_lha.in

## Remove backup files
maintainer-clean-local: maintainer-clean-fc
	-rm -f *~
.PHONY: maintainer-clean-local



