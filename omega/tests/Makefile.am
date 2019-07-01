# Makefile.am -- Makefile for O'Mega within and without WHIZARD
# $Id: Makefile.am 7334 2015-10-26 18:21:22Z bchokoufe $
##
## Process this file with automake to produce Makefile.in
##
########################################################################
#
# Copyright (C) 1999-2015 by
#     Wolfgang Kilian <kilian@physik.uni-siegen.de>
#     Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
#     Juergen Reuter <juergen.reuter@desy.de>
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

SUBDIRS = MSSM SM people

# OMEGA_SPLIT = -target:single_function
  OMEGA_SPLIT = -target:split_function 10
# OMEGA_SPLIT = -target:split_module 10
# OMEGA_SPLIT = -target:split_file 10

OMEGA_QED = $(top_builddir)/bin/omega_QED$(OCAML_NATIVE_EXT)
OMEGA_QED_CACHE = omega_QED.$(OMEGA_CACHE_SUFFIX)
OMEGA_QED_OPTS = $(OMEGA_SPLIT) -target:parameter_module parameters_QED
omega_QED.$(OMEGA_CACHE_SUFFIX): $(OMEGA_QED)
	$(OMEGA_QED) -initialize .

OMEGA_QCD = $(top_builddir)/bin/omega_QCD$(OCAML_NATIVE_EXT)
OMEGA_QCD_CACHE = omega_QCD.$(OMEGA_CACHE_SUFFIX)
OMEGA_QCD_OPTS = $(OMEGA_SPLIT) -target:parameter_module parameters_QCD
omega_QCD.$(OMEGA_CACHE_SUFFIX): $(OMEGA_QCD)
	$(OMEGA_QCD) -initialize .

OMEGA_SYM = $(top_builddir)/bin/omega_SYM$(OCAML_NATIVE_EXT)
OMEGA_SYM_CACHE = omega_SYM.$(OMEGA_CACHE_SUFFIX)
OMEGA_SYM_OPTS = $(OMEGA_SPLIT) -target:parameter_module parameters_SYM
omega_SYM.$(OMEGA_CACHE_SUFFIX): $(OMEGA_SYM)
	$(OMEGA_SYM) -initialize .

OMEGA_SM = $(top_builddir)/bin/omega_SM$(OCAML_NATIVE_EXT)
OMEGA_SM_CACHE = omega_SM.$(OMEGA_CACHE_SUFFIX)
OMEGA_SM_OPTS = $(OMEGA_SPLIT) -target:parameter_module parameters_SM
omega_SM.$(OMEGA_CACHE_SUFFIX): $(OMEGA_SM)
	$(OMEGA_SM) -initialize .

OMEGA_SM_CKM = $(top_builddir)/bin/omega_SM_CKM$(OCAML_NATIVE_EXT)
OMEGA_SM_CKM_CACHE = omega_SM_CKM.$(OMEGA_CACHE_SUFFIX)
omega_SM_CKM.$(OMEGA_CACHE_SUFFIX): $(OMEGA_SM_CKM)
	$(OMEGA_SM_CKM) -initialize .

OMEGA_SM_Higgs = $(top_builddir)/bin/omega_SM_Higgs$(OCAML_NATIVE_EXT)
OMEGA_SM_Higgs_CACHE = omega_SM_Higgs.$(OMEGA_CACHE_SUFFIX)
omega_SM_Higgs.$(OMEGA_CACHE_SUFFIX): $(OMEGA_SM_Higgs)
	$(OMEGA_SM_Higgs) -initialize .

OMEGA_2HDM = $(top_builddir)/bin/omega_2HDM$(OCAML_NATIVE_EXT)
OMEGA_2HDM_CACHE = omega_2HDM.$(OMEGA_CACHE_SUFFIX)
omega_2HDM.$(OMEGA_CACHE_SUFFIX): $(OMEGA_2HDM)
	$(OMEGA_2HDM) -initialize .

OMEGA_2HDM_CKM = $(top_builddir)/bin/omega_2HDM_CKM$(OCAML_NATIVE_EXT)
OMEGA_2HDM_CKM_CACHE = omega_2HDM_CKM.$(OMEGA_CACHE_SUFFIX)
omega_2HDM_CKM.$(OMEGA_CACHE_SUFFIX): $(OMEGA_2HDM_CKM)
	$(OMEGA_2HDM_CKM) -initialize .

OMEGA_HSExt = $(top_builddir)/bin/omega_HSExt$(OCAML_NATIVE_EXT)
OMEGA_HSExt_CACHE = omega_HSExt.$(OMEGA_CACHE_SUFFIX)
omega_HSExt.$(OMEGA_CACHE_SUFFIX): $(OMEGA_HSExt)
	$(OMEGA_HSExt) -initialize .

OMEGA_Zprime = $(top_builddir)/bin/omega_Zprime$(OCAML_NATIVE_EXT)
OMEGA_Zprime_CACHE = omega_Zprime.$(OMEGA_CACHE_SUFFIX)
omega_Zprime.$(OMEGA_CACHE_SUFFIX): $(OMEGA_Zprime)
	$(OMEGA_Zprime) -initialize .

OMEGA_SM_top_anom = $(top_builddir)/bin/omega_SM_top_anom$(OCAML_NATIVE_EXT)
OMEGA_SM_top_anom_CACHE = omega_SM_top_anom.$(OMEGA_CACHE_SUFFIX)
OMEGA_SM_top_anom_OPTS = $(OMEGA_SPLIT) -target:parameter_module parameters_SM_top_anom
omega_SM_top_anom.$(OMEGA_CACHE_SUFFIX): $(OMEGA_SM_top_anom)
	$(OMEGA_SM_top_anom) -initialize .

OMEGA_XXX = $(top_builddir)/bin/omega_%%%$(OCAML_NATIVE_EXT)
OMEGA_XXX_OPTS = -target:parameter_module parameters_%%%

OMEGA_QED_VM = $(top_builddir)/bin/omega_QED_VM$(OCAML_NATIVE_EXT)
OMEGA_QCD_VM = $(top_builddir)/bin/omega_QCD_VM$(OCAML_NATIVE_EXT)
OMEGA_SM_VM = $(top_builddir)/bin/omega_SM_VM$(OCAML_NATIVE_EXT)
OMEGA_SM_CKM_VM = $(top_builddir)/bin/omega_SM_CKM_VM$(OCAML_NATIVE_EXT)
OMEGA_2HDM_VM = $(top_builddir)/bin/omega_2HDM_VM$(OCAML_NATIVE_EXT)
OMEGA_2HDM_CKM_VM = $(top_builddir)/bin/omega_2HDM_CKM_VM$(OCAML_NATIVE_EXT)
OMEGA_HSExt_VM = $(top_builddir)/bin/omega_HSExt_VM$(OCAML_NATIVE_EXT)
OMEGA_Zprime_VM = $(top_builddir)/bin/omega_Zprime_VM$(OCAML_NATIVE_EXT)
OMEGA_SM_Higgs_VM = $(top_builddir)/bin/omega_SM_Higgs_VM$(OCAML_NATIVE_EXT)
OMEGA_XXX_VM = $(top_builddir)/bin/omega_%%%_VM$(OCAML_NATIVE_EXT)
OMEGA_XXX_VM_PARAMS_OPTS = -params -target:parameter_module_external \
	parameters_%%% -target:wrapper_module %% -target:bytecode_file %

AM_FCFLAGS = -I$(top_builddir)/src
AM_LDFLAGS =

########################################################################
## Default Fortran compiler options

## OpenMP
if FC_USE_OPENMP
AM_FCFLAGS += $(FCFLAGS_OPENMP)
endif

########################################################################

TESTS =
XFAIL_TESTS =
EXTRA_PROGRAMS =
EXTRA_DIST =

########################################################################

include $(top_srcdir)/src/Makefile.ocaml

if OCAML_AVAILABLE

OCAMLFLAGS += -I $(top_builddir)/src
OMEGA_CORE = $(top_builddir)/src/omega_core.cmxa
OMEGA_MODELS = $(top_builddir)/src/omega_models.cmxa

TESTS += omega_unit
EXTRA_PROGRAMS += omega_unit

omega_unit_SOURCES = omega_unit.ml

omega_unit: $(OMEGA_CORE) omega_unit.cmx
	$(OCAMLOPT) $(OCAMLFLAGS) $(OCAMLOPTFLAGS) -o omega_unit \
		unix.cmxa $(OMEGA_CORE) omega_unit.cmx

omega_unit.cmx: omega_unit.ml
	$(OCAMLOPT) $(OCAMLFLAGS) $(OCAMLOPTFLAGS) -c -o $@ $<

omega_unit.cmx: $(OMEGA_CORE)

endif

########################################################################

KINDS = $(top_builddir)/src/kinds.lo

TESTS += test_omega95 test_omega95_bispinors
EXTRA_PROGRAMS += test_omega95 test_omega95_bispinors

test_omega95_SOURCES = test_omega95.f90 omega_testtools.f90
test_omega95_LDADD = $(KINDS) $(top_builddir)/src/libomega_core.la

test_omega95_bispinors_SOURCES = test_omega95_bispinors.f90 omega_testtools.f90
test_omega95_bispinors_LDADD = $(KINDS) $(top_builddir)/src/libomega_core.la

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
test_qed_eemm_LDADD = $(KINDS) $(top_builddir)/src/libomega_core.la

amplitude_qed_eemm.f90: $(OMEGA_QED) $(OMEGA_QED_CACHE) Makefile
	$(OMEGA_QED) $(OMEGA_QED_OPTS) -target:module amplitude_qed_eemm \
	-scatter "e+ e- -> m+ m-" > $@

test_qed_eemm.o: amplitude_qed_eemm.o
test_qed_eemm.o: parameters_QED.o
amplitude_qed_eemm.o: parameters_QED.o

########################################################################

EXTENDED_COLOR_TESTS = \
	$(srcdir)/fc_s.ects \
	$(srcdir)/fc_a.ects $(srcdir)/cf_a.ects $(srcdir)/fa_f.ects \
	$(srcdir)/ca_c.ects $(srcdir)/af_f.ects $(srcdir)/ac_c.ects \
	$(srcdir)/aa_a.ects \
	$(srcdir)/fc_fc.ects \
	$(srcdir)/aa_s.ects $(srcdir)/as_a.ects $(srcdir)/sa_a.ects

TESTS += ects
EXTRA_PROGRAMS += ects
EXTRA_DIST += ects_driver.sh $(EXTENDED_COLOR_TESTS)

# Explicitly state dependence on model files

ects.f90: $(OMEGA_QCD) $(OMEGA_SYM) $(OMEGA_SM) $(OMEGA_QCD_CACHE) $(OMEGA_SYM_CACHE) $(OMEGA_SM_CACHE)
ects.f90: ects_driver.sh $(EXTENDED_COLOR_TESTS)
	$(SHELL) $(srcdir)/ects_driver.sh $(OMEGA_XXX) $(EXTENDED_COLOR_TESTS) > $@

ects_SOURCES = color_test_lib.f90 \
	parameters_SM.f90 parameters_QED.f90 parameters_QCD.f90 parameters_SYM.f90
nodist_ects_SOURCES = ects.f90
ects_LDADD = $(KINDS) $(top_builddir)/src/libomega_core.la

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

ward.f90: ward_driver.sh $(OMEGA_QED) $(OMEGA_QCD) $(OMEGA_SYM) \
	$(OMEGA_QED_CACHE) $(OMEGA_QCD_CACHE) $(OMEGA_SYM_CACHE)
ward.f90: $(OMEGA_SM) $(OMEGA_SM_top_anom) \
	$(OMEGA_SM_CACHE) $(OMEGA_SM_top_anom_CACHE)
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
	compare_driver.sh $(OMEGA_QCD) $(OMEGA_QCD_CACHE) $(OMEGA_SM) \
	$(OMEGA_SM_CACHE)

compare_split_function_SOURCES = $(COMPARE_LIB_F90)
nodist_compare_split_function_SOURCES = compare_split_function.f90
compare_split_function_LDADD = $(top_builddir)/src/libomega_core.la

compare_split_module_SOURCES = $(COMPARE_LIB_F90)
nodist_compare_split_module_SOURCES = compare_split_module.f90
compare_split_module_LDADD = $(top_builddir)/src/libomega_core.la

compare_split_function.o compare_split_module.o: $(COMPARE_LIB_O)

########################################################################

TESTS += compare_amplitude_VM
EXTRA_PROGRAMS += compare_amplitude_VM
EXTRA_DIST += compare_driver_VM.sh compare_driver_VM_wrappers.sh
EXTRA_DIST += comparisons_VM.list

compare_amplitude_VM.f90: comparisons_VM.list comparisons_VM.wrappers.o
	rm -f *.hbc
	$(SHELL) $(srcdir)/compare_driver_VM.sh \
	"$(OMEGA_XXX) " "$(OMEGA_XXX_VM) " "$(OMEGA_XXX_VM_PARAMS_OPTS)" < $< > $@

comparisons_VM.wrappers.f90: comparisons_VM.list
	$(SHELL) $(srcdir)/compare_driver_VM_wrappers.sh \
	"$(OMEGA_XXX) " "$(OMEGA_XXX_VM) " "$(OMEGA_XXX_VM_PARAMS_OPTS)" < $< > $@

# Explicitly state dependence on model files
compare_amplitude_VM.f90: compare_driver_VM.sh                   \
	$(OMEGA_QED)      $(OMEGA_QED_VM)      $(OMEGA_QED_CACHE)      \
	$(OMEGA_QCD)      $(OMEGA_QCD_VM)      $(OMEGA_QCD_CACHE)      \
	$(OMEGA_SM)       $(OMEGA_SM_VM)       $(OMEGA_SM_CACHE)       \
	$(OMEGA_SM_CKM)   $(OMEGA_SM_CKM_VM)   $(OMEGA_SM_CKM_CACHE)   \
	$(OMEGA_SM_Higgs) $(OMEGA_SM_Higgs_VM) $(OMEGA_SM_Higgs_CACHE) \
	$(OMEGA_2HDM)     $(OMEGA_2HDM_VM)     $(OMEGA_2HDM_CACHE)     \
	$(OMEGA_2HDM_CKM) $(OMEGA_2HDM_CKM_VM) $(OMEGA_2HDM_CKM_CACHE) \
	$(OMEGA_HSExt)    $(OMEGA_HSExt_VM)    $(OMEGA_HSExt_CACHE)    \
	$(OMEGA_Zprime)   $(OMEGA_Zprime_VM)   $(OMEGA_Zprime_CACHE)

COMPARE_EXTRA_MODELS = parameters_SM_CKM.f90 parameters_SM_Higgs.f90 \
	parameters_2HDM.f90 parameters_2HDM_CKM.f90 parameters_HSExt.f90 \
	parameters_Zprime.f90
compare_amplitude_VM_SOURCES = $(COMPARE_LIB_F90) $(COMPARE_EXTRA_MODELS)
nodist_compare_amplitude_VM_SOURCES = compare_amplitude_VM.f90 comparisons_VM.wrappers.f90
compare_amplitude_VM_LDADD = $(top_builddir)/src/libomega_core.la

compare_amplitude_VM.o: $(COMPARE_LIB_O)

########################################################################

if FC_USE_OPENMP

TESTS += test_openmp
EXTRA_PROGRAMS += test_openmp

TESTOPENMP_SUPPORT_F90 = $(WARD_SUPPORT_F90)
TESTOPENMP_SUPPORT_O = $(WARD_SUPPORT_O)

test_openmp_SOURCES = test_openmp.f90 $(TESTOPENMP_SUPPORT_F90)
nodist_test_openmp_SOURCES = amplitude_openmp.f90
test_openmp_LDADD = $(top_builddir)/src/libomega_core.la

amplitude_openmp.f90: $(OMEGA_QCD) $(OMEGA_QCD_CACHE) Makefile
	$(OMEGA_QCD) $(OMEGA_QCD_OPTS) -target:openmp \
	-target:module amplitude_openmp -scatter "gl gl -> gl gl gl" > $@

test_openmp.o: amplitude_openmp.o
test_openmp.o: $(TESTOPENMP_SUPPORT_O)
amplitude_openmp.o: parameters_QCD.o

endif

########################################################################

EXTRA_PROGRAMS += benchmark_VM_vs_Fortran
EXTRA_DIST += benchmark_VM_vs_Fortran_driver.sh

BENCHMARK_LIB_F90 = benchmark_lib.f90 $(WARD_SUPPORT_F90)
BENCHMARK_LIB_O = $(BENCHMARK_LIB_F90:.f90=.o)

benchmark_VM_vs_Fortran.f90: benchmark_processes.list benchmark_processes.wrappers.o
	$(SHELL) $(srcdir)/benchmark_VM_vs_Fortran_driver.sh \
	"$(OMEGA_XXX) " "$(OMEGA_XXX_VM) " "$(OMEGA_XXX_VM_PARAMS_OPTS)" < $< > $@

benchmark_processes.wrappers.f90: benchmark_processes.list
	$(SHELL) $(srcdir)/benchmark_driver_wrappers.sh \
	"$(OMEGA_XXX) " "$(OMEGA_XXX_VM) " "$(OMEGA_XXX_VM_PARAMS_OPTS)" < $< > $@

# Explicitly state dependence on model files
benchmark_VM_vs_Fortran.f90: benchmark_VM_vs_Fortran_driver.sh \
	$(OMEGA_QED) $(OMEGA_QED_VM) $(OMEGA_QED_CACHE) \
	$(OMEGA_QCD) $(OMEGA_QCD_VM) $(OMEGA_QCD_CACHE) \
	$(OMEGA_SM)  $(OMEGA_SM_VM)  $(OMEGA_SM_CACHE)

benchmark_VM_vs_Fortran_SOURCES = $(BENCHMARK_LIB_F90)
nodist_benchmark_VM_vs_Fortran_SOURCES = benchmark_VM_vs_Fortran.f90 benchmark_processes.wrappers.f90
benchmark_VM_vs_Fortran_LDADD = $(top_builddir)/src/libomega_core.la

benchmark_VM_vs_Fortran.o: $(BENCHMARK_LIB_O)

########################################################################

if FC_USE_OPENMP

EXTRA_PROGRAMS += benchmark_amp_parallel

benchmark_amp_parallel.f90: benchmark_processes.list benchmark_processes.wrappers.o
	$(SHELL) $(srcdir)/benchmark_amp_parallel_driver.sh \
	"$(OMEGA_XXX) " "$(OMEGA_XXX_VM) " "$(OMEGA_XXX_VM_PARAMS_OPTS)" < $< > $@

# Explicitly state dependence on model files
benchmark_amp_parallel.f90: benchmark_amp_parallel_driver.sh \
	$(OMEGA_QED) $(OMEGA_QED_VM) $(OMEGA_QED_CACHE) \
	$(OMEGA_QCD) $(OMEGA_QCD_VM) $(OMEGA_QCD_CACHE) \
	$(OMEGA_SM)  $(OMEGA_SM_VM)  $(OMEGA_SM_CACHE)

benchmark_amp_parallel_SOURCES = $(BENCHMARK_LIB_F90)
nodist_benchmark_amp_parallel_SOURCES = benchmark_amp_parallel.f90 benchmark_processes.wrappers.f90
benchmark_amp_parallel_LDADD = $(top_builddir)/src/libomega_core.la

benchmark_amp_parallel.o: $(BENCHMARK_LIB_O)

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

amplitude_benchmark_v1.f90: $(OMEGA_QCD) $(OMEGA_QCD_CACHE) Makefile
	$(OMEGA_QCD) $(OMEGA_QCD_OPTS) -target:module amplitude_benchmark_v1 \
	$(BENCHMARK_PROCESS) -target:single_function > $@

amplitude_benchmark_v2.f90: $(OMEGA_QCD) $(OMEGA_QCD_CACHE) Makefile
	$(OMEGA_QCD) $(OMEGA_QCD_OPTS) -target:module amplitude_benchmark_v2 \
	$(BENCHMARK_PROCESS) -target:split_function $(BENCHMARK_SPLIT_SIZE) > $@

amplitude_benchmark_v3.f90: $(OMEGA_QCD) $(OMEGA_QCD_CACHE) Makefile
	$(OMEGA_QCD) $(OMEGA_QCD_OPTS) -target:module amplitude_benchmark_v3 \
	$(BENCHMARK_PROCESS) -target:split_module $(BENCHMARK_SPLIT_SIZE) > $@

amplitude_benchmark_v4.f90: $(OMEGA_QCD) $(OMEGA_QCD_CACHE) Makefile
	$(OMEGA_QCD) $(OMEGA_QCD_OPTS) -target:module amplitude_benchmark_v4 \
	$(BENCHMARK_PROCESS) -target:split_file $(BENCHMARK_SPLIT_SIZE) > $@

benchmark.o: \
	amplitude_benchmark_v1.o amplitude_benchmark_v2.o \
	amplitude_benchmark_v3.o # amplitude_benchmark_v4.o
benchmark.o: parameters_QCD.o
amplitude_benchmark_v1.o amplitude_benchmark_v2.o \
	amplitude_benchmark_v3.o amplitude_benchmark_v4.o: parameters_QCD.o

########################################################################

if OCAML_AVAILABLE

TESTS += @comment_model_file@ vertex_unit
@comment_model_file@ EXTRA_PROGRAMS += vertex_unit

@comment_model_file@ vertex_unit_SOURCES = vertex_unit.ml

vertex_unit: $(OMEGA_CORE) vertex_unit.cmx
	$(OCAMLOPT) $(OCAMLFLAGS) $(OCAMLOPTFLAGS) -o vertex_unit \
		unix.cmxa $(OMEGA_CORE) $(OMEGA_MODELS) vertex_unit.cmx

vertex_unit.cmx: vertex_unit.ml
	$(OCAMLOPT) $(OCAMLFLAGS) $(OCAMLOPTFLAGS) -c -o $@ $<

vertex_unit.cmx: $(OMEGA_CORE) $(OMEGA_MODELS)

endif

########################################################################

installcheck-local:
	PATH=$(DESTDIR)$(bindir):$$PATH; export PATH; \
	LD_LIBRARY_PATH=$(DESTDIR)$(libdir):$(DESTDIR)$(pkglibdir):$$LD_LIBRARY_PATH; \
		export LD_LIBRARY_PATH; \
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
	rm -f a.out gmon.out *.$(FC_MODULE_EXT) \
		*.o *.cmi *.cmo *.cmx amplitude_*.f90 *.$(OMEGA_CACHE_SUFFIX) \
		$(EXTRA_PROGRAMS) ects.f90 ward.f90 compare_*.f90 \
		omega_testtools.f90 test_omega95*.f90 benchmark*.f90 \
		*.hbc *wrappers.f90
if FC_SUBMODULES
	-rm -f *.smod
endif

########################################################################
## The End.
########################################################################
