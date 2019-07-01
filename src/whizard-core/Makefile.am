## Makefile.am -- Makefile for WHIZARD
##
## Process this file with automake to produce Makefile.in
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

## The files in this directory make up the WHIZARD core

## We create a library which is still to be combined with auxiliary libs.
noinst_LTLIBRARIES = libwhizard_core.la
check_LTLIBRARIES = libwhizard_core_ut.la

libwhizard_core_la_SOURCES = \
  user_files.f90 \
  rt_data.f90  \
  dispatch.f90 \
  process_configurations.f90 \
  compilations.f90 \
  integrations.f90 \
  event_streams.f90 \
  simulations.f90 \
  commands.f90 \
  whizard.f90 \
  cmdline_options.f90 \
  features.f90

libwhizard_core_ut_la_SOURCES = \
  expr_tests_uti.f90 expr_tests_ut.f90 \
  rt_data_uti.f90 rt_data_ut.f90 \
  dispatch_uti.f90 dispatch_ut.f90 \
  process_configurations_uti.f90 process_configurations_ut.f90 \
  compilations_uti.f90 compilations_ut.f90 \
  integrations_uti.f90 integrations_ut.f90 \
  event_streams_uti.f90 event_streams_ut.f90 \
  simulations_uti.f90 simulations_ut.f90 \
  commands_uti.f90 commands_ut.f90 

# Adds the whizard-c-interface
EXTRA_libwhizard_core_la_SOURCES = whizard-c-interface.f90
libwhizard_core_la_LIBADD = whizard-c-interface.lo

## The main program makes up a library on its own.
lib_LTLIBRARIES = libwhizard_main.la
check_LTLIBRARIES += libwhizard_main_ut.la

libwhizard_main_la_SOURCES = \
  main.f90

libwhizard_main_ut_la_SOURCES = \
  main_ut.f90

## Omitting this would exclude it from the distribution
dist_noinst_DATA = whizard.nw

## Fortran module dependencies
# Get module lists from other directories
module_lists = \
  ../basics/Modules \
  ../utilities/Modules \
  ../testing/Modules \
  ../system/Modules \
  ../combinatorics/Modules \
  ../parsing/Modules \
  ../rng/Modules \
  ../physics/Modules \
  ../qft/Modules \
  ../expr_base/Modules \
  ../sindarin/Modules \
  ../types/Modules \
  ../matrix_elements/Modules \
  ../particles/Modules \
  ../beams/Modules \
  ../me_methods/Modules \
  ../events/Modules \
  ../phase_space/Modules \
  ../mci/Modules \
  ../user/Modules \
  ../blha/Modules \
  ../gosam/Modules \
  ../openloops/Modules \
  ../fks/Modules \
  ../variables/Modules \
  ../model_features/Modules \
  ../muli/Modules \
  ../shower/Modules \
  ../matching/Modules \
  ../process_integration/Modules \
  ../transforms/Modules \
  ../threshold/Modules

$(module_lists):
	$(MAKE) -C `dirname $@` Modules

Module_dependencies.sed: $(module_lists)
	@rm -f $@
	echo 's/, *only:.*//' >> $@
	echo 's/, *&//' >> $@
	echo 's/, *.*=>.*//' >> $@
	echo 's/$$/.lo/' >> $@
	for list in $^; do \
		dir="`dirname $$list`"; \
		for mod in `cat $$list`; do \
			echo 's!: '$$mod'.lo$$!': $$dir/$$mod'.lo!' >> $@; \
		done ; \
	done

DISTCLEANFILES = Module_dependencies.sed

# The following line just says
#    include Makefile.depend
# but in a portable fashion (depending on automake's AM_MAKE_INCLUDE
@am__include@ @am__quote@Makefile.depend@am__quote@

Makefile.depend: Module_dependencies.sed
Makefile.depend: \
  $(libwhizard_core_la_SOURCES) \
  $(libwhizard_core_ut_la_SOURCES) \
  $(libwhizard_main_la_SOURCES) \
  $(libwhizard_main_ut_la_SOURCES) \
  $(EXTRA_libwhizard_core_la_SOURCES)
	@rm -f $@
	for src in $^; do \
		module="`basename $$src | sed 's/\.f[90][0358]//'`"; \
		grep '^ *use ' $$src \
			| grep -v '!NODEP!' \
			| sed -e 's/^ *use */'$$module'.lo: /' \
			-f Module_dependencies.sed; \
	done > $@

DISTCLEANFILES += Makefile.depend

SUFFIXES = .lo .$(FC_MODULE_EXT)
# Fortran90 module files are generated at the same time as object files
.lo.$(FC_MODULE_EXT):
	@:
#	touch $@

AM_FCFLAGS = -I../basics -I../utilities -I../testing -I../system -I../combinatorics -I../parsing -I../rng -I../physics -I../qft -I../expr_base -I../sindarin -I../types -I../matrix_elements -I../particles -I../beams -I../me_methods -I../events -I../phase_space -I../mci -I../user -I../blha -I../gosam -I../openloops -I../fks -I../variables -I../model_features -I../muli -I../shower -I../matching -I../process_integration -I../transforms -I../xdr -I../../vamp/src -I../pdf_builtin -I../../circe1/src -I../../circe2/src -I../lhapdf -I../fastjet -I../threshold -I../tauola

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

## (Re)create F90 sources from NOWEB source.
if NOWEB_AVAILABLE

WHIZARD_SRC = \
  $(libwhizard_core_la_SOURCES) \
  $(libwhizard_core_ut_la_SOURCES) \
  $(libwhizard_main_la_SOURCES) \
  $(libwhizard_main_ut_la_SOURCES) \
  $(EXTRA_libwhizard_core_la_SOURCES)

PRELUDE = $(top_srcdir)/src/noweb-frame/whizard-prelude.nw
POSTLUDE = $(top_srcdir)/src/noweb-frame/whizard-postlude.nw

whizard.stamp: $(PRELUDE) $(srcdir)/whizard.nw $(POSTLUDE)
	@rm -f whizard.tmp
	@touch whizard.tmp
	for src in $(WHIZARD_SRC); do \
	  $(NOTANGLE) -R[[$$src]] $^ | $(CPIF) $$src; \
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
if FC_SUBMODULES
	-rm -f *.smod
endif

## Remove backup files
maintainer-clean-backup:
	-rm -f *~
.PHONY: maintainer-clean-backup

## Register additional clean targets
maintainer-clean-local: maintainer-clean-noweb maintainer-clean-backup
