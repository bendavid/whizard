## Makefile.am -- Makefile for WHIZARD
##
## Process this file with automake to produce Makefile.in
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

## The files in this directory make up the WHIZARD core

## We create a library which is still to be combined with auxiliary libs.
## 
noinst_LTLIBRARIES = libwhizard_core.la

libwhizard_core_la_SOURCES = \
  unit_tests.f90 \
  os_interface.f90 cputime.f90 \
  hashes.f90 sprintf_interface.c formats.f90 bytes.f90 md5.f90 \
  permutations.f90 sorting.f90 ifiles.f90 \
  lexers.f90 syntax_rules.f90 parser.f90 xml.f90 \
  user_files.f90 analysis.f90 \
  jets.f90 pdg_arrays.f90 subevents.f90 \
  user_code_interface.f90 \
  variables.f90 expressions.f90 \
  models.f90 auto_components.f90 \
  helicities.f90 colors.f90 flavors.f90 quantum_numbers.f90 \
  state_matrices.f90 interactions.f90 evaluators.f90 \
  polarizations.f90 particles.f90 \
  beam_structures.f90 beams.f90 \
  sm_qcd.f90 \
  sf_aux.f90 sf_mappings.f90 sf_base.f90 \
  sf_pdf_builtin.f90 sf_lhapdf.f90 sf_circe1.f90 \
  sf_circe2.f90 sf_isr.f90 sf_epa.f90 sf_ewa.f90 \
  sf_escan.f90 sf_beam_events.f90 sf_user.f90 \
  process_constants.f90 \
  phs_base.f90 phs_single.f90 \
  mappings.f90 phs_trees.f90 phs_forests.f90 cascades.f90 phs_wood.f90 \
  rng_base.f90 rng_tao.f90 selectors.f90 \
  mci_base.f90 mci_midpoint.f90 mci_vamp.f90 \
  prclib_interfaces.f90 \
  prc_core_def.f90 particle_specifiers.f90 \
  process_libraries.f90 prclib_stacks.f90 \
  prc_test.f90 \
  integration_results.f90 \
  prc_core.f90 subevt_expr.f90 parton_states.f90 \
  processes.f90 process_stacks.f90 \
  event_transforms.f90 decays.f90 shower.f90 \
  events.f90 prc_omega.f90 prc_template_me.f90 \
  hep_common.f90 hep_events.f90 hepmc_interface.f90 \
  hoppet_interface.f90 \
  eio_data.f90 eio_base.f90 eio_raw.f90 eio_checkpoints.f90 \
  eio_lhef.f90 \
  eio_hepmc.f90 eio_ascii.f90 eio_stdhep.f90 eio_weights.f90 \
  iterations.f90 rt_data.f90  \
  blha_config.f90 blha_interface.f90 blha_driver.f90 \
  slha_interface.f90 dispatch.f90 \
  process_configurations.f90 compilations.f90 integrations.f90 \
  event_streams.f90 simulations.f90 \
  commands.f90 \
  whizard.f90
### Old NLO files
#  nlo_setup.f90 dipoles_integrated_qed.f90 dipoles_real_qed.f90\
  photon_recombination.f90 \
  core_interactions.f90 core_interactions_config.f90
# Adds the whizard-c-interface
EXTRA_libwhizard_core_la_SOURCES = whizard-c-interface.f90
libwhizard_core_la_LIBADD = whizard-c-interface.lo

## This C file has no dependencies
sprintf_interface.c:

## The main program makes up a library on its own.
lib_LTLIBRARIES = libwhizard_main.la

libwhizard_main_la_SOURCES = main.f90

## Omitting this would exclude it from the distribution
dist_noinst_DATA = whizard.nw

## Fortran module dependencies
# The following line just says
#    include Makefile.depend
# but in a portable fashion (depending on automake's AM_MAKE_INCLUDE
@am__include@ @am__quote@Makefile.depend@am__quote@

Makefile.depend: $(libwhizard_core_la_SOURCES) $(libwhizard_main_la_SOURCES) $(EXTRA_libwhizard_core_la_SOURCES)
	@rm -f $@
	for src in $^; do \
	  module="`basename $$src | sed 's/\.f[90][0358]//'`"; \
	  grep '^ *use ' $$src \
	    | grep -v '!NODEP!' \
	    | sed -e 's/^ *use */'$$module'.lo: /' \
	          -e 's/, *only:.*//' \
	          -e 's/, *&//' \
	          -e 's/, *.*=>.*//' \
	          -e 's/$$/.lo/' ; \
	done > $@

DISTCLEANFILES = Makefile.depend 

## Dependency libaux (kinds, constants, strings) and VAMP, CIRCE1, CIRCE2
$(libwhizard_core_la_OBJECTS): \
  ../misc/kinds.$(FC_MODULE_EXT) \
  ../misc/constants.$(FC_MODULE_EXT) \
  ../misc/iso_varying_string.$(FC_MODULE_EXT) \
  ../misc/system_dependencies.$(FC_MODULE_EXT) \
  ../misc/limits.$(FC_MODULE_EXT) \
  ../misc/file_utils.$(FC_MODULE_EXT) \
  ../misc/diagnostics.$(FC_MODULE_EXT) \
  ../misc/c_particles.$(FC_MODULE_EXT) \
  ../misc/lorentz.$(FC_MODULE_EXT) \
  ../misc/sm_physics.$(FC_MODULE_EXT) \
  ../lhapdf/lhapdf.$(FC_MODULE_EXT) \
  ../../vamp/src/vamp.$(FC_MODULE_EXT) \
  ../../circe1/src/circe1.$(FC_MODULE_EXT) \
  ../../circe2/src/circe2.$(FC_MODULE_EXT)
## Dependency libshower (kinds, constants, strings) and VAMP
#$(libwhizard_core_la_OBJECTS): \
  ../shower/shower_module.$(FC_MODULE_EXT) \
  ../shower/shower_particle.$(FC_MODULE_EXT) \
  ../shower/shower_basics.$(FC_MODULE_EXT) \
  ../pdf_builtin/pdf_builtin.$(FC_MODULE_EXT) \
  ../muli/muli.$(FC_MODULE_EXT)

SUFFIXES = .lo .$(FC_MODULE_EXT)
# Fortran90 module files are generated at the same time as object files
.lo.$(FC_MODULE_EXT):
	@:
#	touch $@

AM_FCFLAGS = -I../misc -I../../vamp/src -I../shower -I../pdf_builtin -I../muli -I../../circe1/src -I../../circe2/src -I../lhapdf -I../fastjet

########################################################################
## Default Fortran compiler options

## Profiling
if FC_USE_PROFILING
AM_FCFLAGS += $(FCFLAGS_PROFILING)
endif

## OpenMP
if FC_USE_OPENMP
AM_FCFLAGS += $(FCFLAGS_OPENMP)
endif

########################################################################
## Non-standard targets and dependencies

## Install the modules used by generated matrix element code
## and by the user code library
execmoddir = $(pkglibdir)/mod/whizard-core
nodist_execmod_HEADERS = \
  unit_tests.$(FC_MODULE_EXT) \
  os_interface.$(FC_MODULE_EXT) \
  user_code_interface.$(FC_MODULE_EXT) \
  prclib_interfaces.$(FC_MODULE_EXT)

## (Re)create F90 sources from NOWEB source.
if NOWEB_AVAILABLE

WHIZARD_SRC = $(libwhizard_core_la_SOURCES) $(EXTRA_libwhizard_core_la_SOURCES) main.f90

whizard.stamp: $(srcdir)/whizard.nw
	@rm -f whizard.tmp
	@touch whizard.tmp
	for src in $(WHIZARD_SRC); do \
	  $(NOTANGLE) -R[[$$src]] $< | $(CPIF) $$src; \
        done
	@mv -f whizard.tmp whizard.stamp

$(WHIZARD_SRC): whizard.stamp
## Recover from the removal of $@
	@if test -f $@; then :; else \
	  rm -f whizard.stamp; \
	  $(MAKE) $(AM_MAKEFLAGS) whizard.stamp; \
	fi

endif

########################################################################
## Non-standard cleanup tasks
## Remove sources that can be recreated using NOWEB
if NOWEB_AVAILABLE
maintainer-clean-noweb:
	-rm -f *.f90 *.c
endif
.PHONY: maintainer-clean-noweb

## Remove those sources also if builddir and srcdir are different
if NOWEB_AVAILABLE
clean-noweb:
	test "$(srcdir)" != "." && rm -f *.f90 *.c || true
endif
.PHONY: clean-noweb

## Remove F90 module files
clean-local: clean-noweb
	-rm -f whizard.stamp whizard.tmp
	-rm -f *.$(FC_MODULE_EXT)

## Remove backup files
maintainer-clean-backup:
	-rm -f *~
.PHONY: maintainer-clean-backup

## Register additional clean targets
maintainer-clean-local: maintainer-clean-noweb maintainer-clean-backup
