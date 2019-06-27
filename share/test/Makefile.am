## Makefile.am -- Makefile for WHIZARD tests
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

EXTRA_DIST = \
    $(TESTSUITE_MACROS) $(TESTSUITES_M4) $(TESTSUITES_SIN) \
    susyhit.in TEST.mdl

TESTSUITE_MACROS = testsuite.m4

TESTSUITES_M4 = \
    $(MISC_TESTS_M4) \
    $(MSSM_EXT_M4) \
    $(NMSSM_EXT_M4)

TESTSUITES_SIN = \
    $(MISC_TESTS_SIN) \
    $(MSSM_EXT_SIN) \
    $(NMSSM_EXT_SIN)

MISC_TESTS_M4 =

MISC_TESTS_SIN = \
    empty.sin vars.sin errors.sin md5.sin \
    alphas.sin \
    colors.sin \
    cuts.sin \
    decays.sin \
    defaultcuts.sin \
    extpar.sin \
    helicity.sin \
    ilc.sin \
    jets.sin \
    lhapdf.sin \
    libs.sin \
    mssmtest.sin \
    plain.sin \
    qedtest.sin \
    restrictions.sin \
    stdhep.sin \
    stdhep_up.sin \
    set_ptcut.sin \
    smtest.sin \
    static.sin static2.sin \
    susyhit.sin \
    sm_ext.sin \
    models.sin

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

