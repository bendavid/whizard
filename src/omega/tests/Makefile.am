# Makefile.am -- Makefile for O'Mega within and without WHIZARD
# $Id: Makefile.am 3930 2012-09-09 18:48:11Z jr_reuter $
##
## Process this file with automake to produce Makefile.in
##
########################################################################
#
# Copyright (C) 1999-2012 by 
#     Wolfgang Kilian <kilian@physik.uni-siegen.de>
#     Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
#     Juergen Reuter <juergen.reuter@desy.de>
#     Christian Speckner <christian.speckner@physik.uni-freiburg.de>
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

SUBDIRS = MSSM SM people

# OMEGA_SPLIT = -target:single_function
  OMEGA_SPLIT = -target:split_function 10
# OMEGA_SPLIT = -target:split_module 10
# OMEGA_SPLIT = -target:split_file 10

OMEGA_QED = $(top_builddir)/bin/omega_QED.opt
OMEGA_QED_OPTS = $(OMEGA_SPLIT) -target:parameter_module parameters_QED

OMEGA_QCD = $(top_builddir)/bin/omega_QCD.opt
OMEGA_QCD_OPTS = $(OMEGA_SPLIT) -target:parameter_module parameters_QCD

OMEGA_SYM = $(top_builddir)/bin/omega_SYM.opt
OMEGA_SYM_OPTS = $(OMEGA_SPLIT) -target:parameter_module parameters_SYM

OMEGA_XXX = $(top_builddir)/bin/omega_%%%.opt
OMEGA_XXX_OPTS = -target:parameter_module parameters_%%%

AM_FCFLAGS = -I$(top_builddir)/src
AM_LDFLAGS =

########################################################################
## Default Fortran compiler options

## OpenMP
if FC_USE_OPENMP
AM_FCFLAGS += $(FCFLAGS_OPENMP)
endif

########################################################################

TESTS = test_omega95 test_omega95_bispinors
XFAIL_TESTS =
EXTRA_PROGRAMS = test_omega95 test_omega95_bispinors
EXTRA_DIST = 

test_omega95_SOURCES = test_omega95.f90 omega_testtools.f90
test_omega95_LDADD = $(top_builddir)/src/libomega_core.la

test_omega95_bispinors_SOURCES = test_omega95_bispinors.f90 omega_testtools.f90
test_omega95_bispinors_LDADD = $(top_builddir)/src/libomega_core.la

test_omega95.o test_omega95_bispinors.o: omega_testtools.o

if NOWEB_AVAILABLE

test_omega95.f90: $(top_srcdir)/src/omegalib.nw
	$(NOTANGLE) -R[[$@]] $< | $(CPIF) $@
test_omega95_bispinors.f90: $(top_srcdir)/src/omegalib.nw
	$(NOTANGLE) -R[[$@]] $< | $(CPIF) $@
omega_testtools.f90: $(top_srcdir)/src/omegalib.nw
	$(NOTANGLE) -R[[$@]] $< | $(CPIF) $@

endif NOWEB_AVAILABLE

########################################################################

TESTS += test_qed_eemm
EXTRA_PROGRAMS += test_qed_eemm

test_qed_eemm_SOURCES = test_qed_eemm.f90 parameters_QED.f90
nodist_test_qed_eemm_SOURCES = amplitude_qed_eemm.f90
test_qed_eemm_LDADD = $(top_builddir)/src/libomega_core.la

amplitude_qed_eemm.f90: $(OMEGA_QED) Makefile
	$(OMEGA_QED) $(OMEGA_QED_OPTS) -target:module amplitude_qed_eemm \
	  -scatter "e+ e- -> m+ m-" > $@

test_qed_eemm.o: amplitude_qed_eemm.o
test_qed_eemm.o: parameters_QED.o
amplitude_qed_eemm.o: parameters_QED.o

########################################################################

EXTENDED_COLOR_TESTS = \
    fc_s.ects \
    fc_a.ects cf_a.ects fa_f.ects ca_c.ects af_f.ects ac_c.ects \
    aa_a.ects \
    fc_fc.ects \
    aa_s.ects as_a.ects sa_a.ects

TESTS += ects
EXTRA_PROGRAMS += ects
EXTRA_DIST += ects_driver.sh $(EXTENDED_COLOR_TESTS)

ects.f90: ects_driver.sh $(EXTENDED_COLOR_TESTS)
	$(SHELL) $(srcdir)/ects_driver.sh $(OMEGA_XXX) $^ > $@

ects_SOURCES = color_test_lib.f90 \
    parameters_SM.f90 parameters_QED.f90 parameters_QCD.f90 parameters_SYM.f90
nodist_ects_SOURCES = ects.f90
ects_LDADD = $(top_builddir)/src/libomega_core.la

########################################################################

TESTS += ward
EXTRA_PROGRAMS += ward
EXTRA_DIST += ward_driver.sh
EXTRA_DIST += ward_identities.list

WARD_SUPPORT_F90 = \
    omega_interface.f90 omega_testtools.f90 tao_random_numbers.f90 \
    parameters_QED.f90 parameters_QCD.f90 parameters_SYM.f90 \
    parameters_SM.f90 parameters_SM_top_anom.f90
WARD_SUPPORT_O = $(WARD_SUPPORT_F90:.f90=.o)
ward_lib.o: $(WARD_SUPPORT_O)

WARD_LIB_F90 = ward_lib.f90 $(WARD_SUPPORT_F90)
WARD_LIB_O = $(WARD_LIB_F90:.f90=.o)

run_ward: ward
	./ward

ward.f90: ward_driver.sh $(OMEGA_QED) $(OMEGA_QCD) $(OMEGA_SYM)
ward.f90: ward_identities.list
	$(SHELL) $(srcdir)/ward_driver.sh $(OMEGA_XXX) $(OMEGA_SPLIT) < $< > $@

ward_SOURCES = $(WARD_LIB_F90)
nodist_ward_SOURCES = ward.f90 
ward_LDADD = $(top_builddir)/src/libomega_core.la

ward.o: $(WARD_LIB_O)

########################################################################

EXTRA_PROGRAMS += ward_long
EXTRA_DIST += ward_identities_long.list

run_ward_long: ward_long
	./ward_long

ward_long.f90: ward_driver.sh
ward_long.f90: ward_identities_long.list
	$(SHELL) $(srcdir)/ward_driver.sh $(OMEGA_XXX) $(OMEGA_SPLIT) < $< > $@

ward_long_SOURCES = $(WARD_LIB_F90)
nodist_ward_long_SOURCES = ward_long.f90 
ward_long_LDADD = $(top_builddir)/src/libomega_core.la

# ward_long.o: ward_long.f90
# 	$(FCCOMPILE) -c -o $@ $(FCFLAGS_f90) -O0 $<

ward_long.o: $(WARD_LIB_O)

########################################################################

EXTRA_PROGRAMS += ward_fail
EXTRA_DIST += ward_identities_fail.list

run_ward_fail: ward_fail
	./ward_fail

ward_fail.f90: ward_driver.sh
ward_fail.f90: ward_identities_fail.list
	$(SHELL) $(srcdir)/ward_driver.sh $(OMEGA_XXX) $(OMEGA_SPLIT) < $< > $@

ward_fail_SOURCES = $(WARD_LIB_F90)
nodist_ward_fail_SOURCES = ward_fail.f90 
ward_fail_LDADD = $(top_builddir)/src/libomega_core.la

ward_fail.o: ward_fail.f90
	$(FCCOMPILE) -c -o $@ $(FCFLAGS_f90) -O0 $<

ward_fail.o: $(WARD_LIB_O)

######################################################################## 

TESTS += compare_split_function compare_split_module
EXTRA_PROGRAMS += compare_split_function compare_split_module
EXTRA_DIST += compare_driver.sh
EXTRA_DIST += comparisons.list

COMPARE_SUPPORT_F90 = $(WARD_SUPPORT_F90)
COMPARE_SUPPORT_O = $(WARD_SUPPORT_O)
compare_lib.o: $(COMPARE_SUPPORT_O)

COMPARE_LIB_F90 = compare_lib.f90 $(COMPARE_SUPPORT_F90)
COMPARE_LIB_O = $(COMPARE_LIB_F90:.f90=.o)

run_compare: compare_split_function compare_split_module
	./compare_split_function
	./compare_split_module

compare_split_function.f90: comparisons.list
	$(SHELL) $(srcdir)/compare_driver.sh SF \
	  "$(OMEGA_XXX) -target:single_function" \
	  "$(OMEGA_XXX) -target:split_function 10" < $< > $@

compare_split_module.f90: comparisons.list
	$(SHELL) $(srcdir)/compare_driver.sh SM \
	  "$(OMEGA_XXX) -target:single_function" \
	   "$(OMEGA_XXX) -target:split_module 10" < $< > $@

compare_split_function.f90 compare_split_module.f90: \
    compare_driver.sh $(OMEGA_QED) $(OMEGA_QCD) $(OMEGA_SYM)

compare_split_function_SOURCES = $(COMPARE_LIB_F90)
nodist_compare_split_function_SOURCES = compare_split_function.f90 
compare_split_function_LDADD = $(top_builddir)/src/libomega_core.la

compare_split_module_SOURCES = $(COMPARE_LIB_F90)
nodist_compare_split_module_SOURCES = compare_split_module.f90 
compare_split_module_LDADD = $(top_builddir)/src/libomega_core.la

compare_split_function.o compare_split_module.o: $(COMPARE_LIB_O)

######################################################################## 

if FC_USE_OPENMP
TESTS += test_openmp
EXTRA_PROGRAMS += test_openmp

test_openmp_SOURCES = test_openmp.f90 parameters_QCD.f90
nodist_test_openmp_SOURCES = amplitude_openmp.f90
test_openmp_LDADD = $(top_builddir)/src/libomega_core.la

amplitude_openmp.f90: $(OMEGA_QCD) Makefile
	$(OMEGA_QCD) $(OMEGA_QCD_OPTS) -target:openmp -target:module amplitude_openmp \
	  -scatter "gl gl -> gl gl gl" > $@

test_openmp.o: amplitude_openmp.o
test_openmp.o: parameters_QCD.o
amplitude_openmp.o: parameters_QCD.o

endif

######################################################################## 

EXTRA_PROGRAMS += benchmark

run_benchmark: benchmark
	./benchmark

BENCHMARK_PROCESS = -scatter "gl gl -> gl gl gl"
BENCHMARK_SPLIT_SIZE = 10

benchmark_SOURCES = benchmark.f90 parameters_QCD.f90
nodist_benchmark_SOURCES = \
    amplitude_benchmark_v1.f90 amplitude_benchmark_v2.f90 \
    amplitude_benchmark_v3.f90 # amplitude_benchmark_v4.f90
benchmark_LDADD = $(top_builddir)/src/libomega_core.la

amplitude_benchmark_v1.f90: $(OMEGA_QCD) Makefile
	$(OMEGA_QCD) $(OMEGA_QCD_OPTS) -target:module amplitude_benchmark_v1 \
	  $(BENCHMARK_PROCESS) -target:single_function > $@

amplitude_benchmark_v2.f90: $(OMEGA_QCD) Makefile
	$(OMEGA_QCD) $(OMEGA_QCD_OPTS) -target:module amplitude_benchmark_v2 \
	  $(BENCHMARK_PROCESS) -target:split_function $(BENCHMARK_SPLIT_SIZE) > $@

amplitude_benchmark_v3.f90: $(OMEGA_QCD) Makefile
	$(OMEGA_QCD) $(OMEGA_QCD_OPTS) -target:module amplitude_benchmark_v3 \
	  $(BENCHMARK_PROCESS) -target:split_module $(BENCHMARK_SPLIT_SIZE) > $@

amplitude_benchmark_v4.f90: $(OMEGA_QCD) Makefile
	$(OMEGA_QCD) $(OMEGA_QCD_OPTS) -target:module amplitude_benchmark_v4 \
	  $(BENCHMARK_PROCESS) -target:split_file $(BENCHMARK_SPLIT_SIZE) > $@

benchmark.o: \
    amplitude_benchmark_v1.o amplitude_benchmark_v2.o \
    amplitude_benchmark_v3.o # amplitude_benchmark_v4.o
benchmark.o: parameters_QCD.o
amplitude_benchmark_v1.o amplitude_benchmark_v2.o \
    amplitude_benchmark_v3.o amplitude_benchmark_v4.o: parameters_QCD.o

########################################################################

installcheck-local:
	PATH=$(DESTDIR)$(bindir):$$PATH; export PATH; \
	LD_LIBRARY_PATH=$(DESTDIR)$(libdir):$(DESTDIR)$(pkglibdir):$$LD_LIBRARY_PATH; export LD_LIBRARY_PATH; \
	omega_QED.opt $(OMEGA_QED_OPTS) -scatter "e+ e- -> m+ m-" \
	  -target:module amplitude_qed_eemm > amplitude_qed_eemm.f90; \
	$(FC) $(AM_FCFLAGS) $(FCFLAGS) -I$(pkgincludedir) \
          -L$(DESTDIR)$(libdir) -L$(DESTDIR)$(pkglibdir) \
	  $(srcdir)/parameters_QED.f90 amplitude_qed_eemm.f90 \
	  $(srcdir)/test_qed_eemm.f90 -lomega_core; \
	./a.out

########################################################################

### Remove DWARF debug information on MAC OS X
clean-macosx:
	-rm -rf a.out.dSYM
.PHONY: clean-macosx

clean-local: clean-macosx
	rm -f a.out gmon.out $(OMEGA_CACHES) *.$(FC_MODULE_EXT) *.o amplitude_*.f90 \
	  $(EXTRA_PROGRAMS) ects.f90 ward.f90 compare_split_*.f90 \
	  omega_testtools.f90 test_omega95*.f90

########################################################################
## The End.
########################################################################
