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
#     Sebastian Schmidt, Daniel Wiesler
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
if SHOWER_AVAILABLE
noinst_LTLIBRARIES = libshower.la
else
noinst_LTLIBRARIES = libshower_dummy.la
endif

## If shower is enabled, pythia is enabled automatically

AM_FFLAGS =
AM_FCFLAGS =

if FC_IS_NAG
AM_FFLAGS += -dcfuns -w
AM_FCFLAGS += -dcfuns -w
endif

libshower_la_SOURCES = \
pythia_up.f shower_base.f90 shower_partons.f90 \
shower_core.f90 shower_topythia.f90 mlm_matching.f90 \
ckkw_pseudo_weights.f90 ckkw_matching.f90

libshower_dummy_la_SOURCES = shower_dummy.f90

## Omitting this would exclude it from the distribution
dist_noinst_DATA = shower.nw

## Fortran module dependencies
# The following line just says
#    include Makefile.depend
# but in a portable fashion (depending on automake's AM_MAKE_INCLUDE
@am__include@ @am__quote@Makefile.depend@am__quote@

Makefile.depend: $(libshower_la_SOURCES)
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

## Dependency libaux (kinds, strings)
## (tao_random_numbers will be removed when switching to rng_factory)
$(libshower_la_OBJECTS): \
../misc/kinds.$(FC_MODULE_EXT) \
../misc/constants.$(FC_MODULE_EXT) \
../misc/file_utils.$(FC_MODULE_EXT) \
../misc/diagnostics.$(FC_MODULE_EXT) \
../misc/lorentz.$(FC_MODULE_EXT) \
../vamp/src/tao_random_numbers.$(FC_MODULE_EXT)

libshower_la_LIBADD = ../pythia/libpythia_wo.la

SUFFIXES = .lo .$(FC_MODULE_EXT)
# Fortran90 module files are generated at the same time as object files
.lo.$(FC_MODULE_EXT):
        @:
#       touch $@

AM_FCFLAGS += -I../misc -I../vamp/src/ -I../pythia

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
if NOWEB_AVAILABLE

SHOWER_SRC = $(libshower_la_SOURCES) $(libshower_dummy_la_SOURCES) 

shower.stamp: $(srcdir)/shower.nw
	@rm -f shower.tmp
	@touch shower.tmp
	for src in $(SHOWER_SRC); do \
	   $(NOTANGLE) -R[[$$src]] $< | $(CPIF) $$src; \
	done
	@mv -f shower.tmp shower.stamp

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
