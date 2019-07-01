## Makefile.am -- Makefile for WHIZARD
##
## Process this file with automake to produce Makefile.in
#
# Copyright (C) 1999-2015 by
#     Wolfgang Kilian <kilian@physik.uni-siegen.de>
#     Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
#     Juergen Reuter <juergen.reuter@desy.de>
#     with contributions from
#     Fabian Bach <fabian.bach@desy.de>
#     Bijan Chokoufe <bijan.chokoufe@desy.de>
#     Christian Speckner <cnspeckn@googlemail.com>
#     Marco Sekulla <marco.sekulla@desy.de>
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

## The files in this directory end up in an auxiliary libtool library.
noinst_LTLIBRARIES = libshower.la

libshower_la_SOURCES = \
   pythia6_up.f \
   shower_base.f90 \
   shower_partons.f90 \
   shower_core.f90 \
   shower_topythia.f90 \
   mlm_matching.f90 \
   ckkw_pseudo_weights.f90 \
   ckkw_matching.f90

## Omitting this would exclude it from the distribution
dist_noinst_DATA = shower.nw

# Dump module names into file Modules
libshower_Modules = ${libshower_la_SOURCES:.f90=}
Modules: Makefile
	@rm -f $@
	for module in $(libshower_Modules); do \
          echo $$module >> $@; \
        done

## Fortran module dependencies
# Get module lists from other directories
module_lists = \
  ../basics/Modules \
  ../utilities/Modules \
  ../testing/Modules \
  ../system/Modules \
  ../combinatorics/Modules \
  ../rng/Modules \
  ../physics/Modules \
  ../qft/Modules \
  ../types/Modules \
  ../particles/Modules \
  ../events/Modules

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
Makefile.depend: $(libshower_la_SOURCES)
	@rm -f $@
	for src in $^; do \
		module="`basename $$src | sed 's/\.f[90][0358]//'`"; \
		grep '^ *use ' $$src \
			| grep -v '!NODEP!' \
			| sed -e 's/^ *use */'$$module'.lo: /' \
			-f Module_dependencies.sed; \
	done > $@

DISTCLEANFILES += Makefile.depend

if PYTHIA6_AVAILABLE
libshower_la_LIBADD = ../../pythia6/libpythia6_wo.la
else
libshower_la_LIBADD = ../../pythia6/libpythia6_wo_dummy.la
endif

# Fortran90 module files are generated at the same time as object files
.lo.$(FC_MODULE_EXT):
	@:
#	touch $@

AM_FCFLAGS = -I../basics -I../utilities -I ../system -I../testing -I ../rng -I../physics -I../fastjet -I../qft -I../types -I../particles -I../events -I ../lhapdf -I ../pdf_builtin -I../../pythia6
AM_FFLAGS =

########################################################################
## Default Fortran compiler options

## Profiling
if FC_USE_PROFILING
AM_FFLAGS += $(FCFLAGS_PROFILING)
AM_FCFLAGS += $(FCFLAGS_PROFILING)
endif

## OpenMP
if FC_USE_OPENMP
AM_FFLAGS += $(FCFLAGS_OPENMP)
AM_FCFLAGS += $(FCFLAGS_OPENMP)
endif

########################################################################

## (Re)create F90 sources from NOWEB source.
## This needs to be outside the NOWEB bracket.

if NOWEB_AVAILABLE

PRELUDE = $(top_srcdir)/src/noweb-frame/whizard-prelude.nw
POSTLUDE = $(top_srcdir)/src/noweb-frame/whizard-postlude.nw

shower.stamp: $(PRELUDE) $(srcdir)/shower.nw $(POSTLUDE)
	@rm -f shower.tmp
	@touch shower.tmp
	for src in $(SHOWER_SRC); do \
		$(NOTANGLE) -R[[$$src]] $^ | $(CPIF) $$src; \
	done
	@mv -f shower.tmp shower.stamp

SHOWER_SRC = $(libshower_la_SOURCES) 

$(SHOWER_SRC): shower.stamp
## Recover from the removal of $@
	@if test -f $@; then :; else \
		rm -f shower.stamp; \
		$(MAKE) $(AM_MAKEFLAGS) shower.stamp; \
	fi

endif

########################################################################
## Non-standard cleanup tasks
## Remove sources that can be recreated using NOWEB
if NOWEB_AVAILABLE
maintainer-clean-noweb:
	-rm -f *.f90 *.f
endif
.PHONY: maintainer-clean-noweb

## Remove those sources also if builddir and srcdir are different
if NOWEB_AVAILABLE
clean-noweb:
	test "$(srcdir)" != "." && rm -f *.f90 *.f || true
endif
.PHONY: clean-noweb

## Remove F90 module files
clean-local: clean-noweb
	-rm -f shower.stamp shower.tmp
	-rm -f *.$(FC_MODULE_EXT)

## Remove backup files
maintainer-clean-backup:
	-rm -f *~
.PHONY: maintainer-clean-backup

## Register additional clean targets
maintainer-clean-local: maintainer-clean-noweb maintainer-clean-backup
