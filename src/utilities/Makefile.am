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

## The files in this directory are simple utilities used by WHIZARD

## We create a library which is still to be combined with auxiliary libs.
noinst_LTLIBRARIES = libutilities.la

libutilities_la_SOURCES = \
  file_utils.f90 \
  string_utils.f90 \
  format_utils.f90 \
  format_defs.f90

## Omitting this would exclude it from the distribution
dist_noinst_DATA = utilities.nw

# Dump module names into file Modules
libutilities_Modules = ${libutilities_la_SOURCES:.f90=}
Modules: Makefile
	@rm -f $@
	for module in $(libutilities_Modules); do \
          echo $$module >> $@; \
        done

## Fortran module dependencies
# Get module lists from other directories
module_lists = \
  ../basics/Modules

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
Makefile.depend: $(libutilities_la_SOURCES)
	@rm -f $@
	for src in $^; do \
	  module="`basename $$src | sed 's/\.f[90][0358]//'`"; \
	  grep '^ *use ' $$src \
	    | grep -v '!NODEP!' \
	    | sed -e 's/^ *use */'$$module'.lo: /' \
	          -f Module_dependencies.sed; \
	done > $@

DISTCLEANFILES += Makefile.depend 

# Fortran90 module files are generated at the same time as object files
.lo.$(FC_MODULE_EXT):
	@:
#	touch $@

AM_FCFLAGS = -I../basics


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
execmoddir = $(pkglibdir)/mod/utilities
nodist_execmod_HEADERS = \
  file_utils.$(FC_MODULE_EXT) \
  string_utils.$(FC_MODULE_EXT) \
  format_utils.$(FC_MODULE_EXT) \
  format_defs.$(FC_MODULE_EXT)

## (Re)create F90 sources from NOWEB source.
if NOWEB_AVAILABLE

PRELUDE = $(top_srcdir)/src/noweb-frame/whizard-prelude.nw
POSTLUDE = $(top_srcdir)/src/noweb-frame/whizard-postlude.nw

utilities.stamp: $(PRELUDE) $(srcdir)/utilities.nw $(POSTLUDE)
	@rm -f utilities.tmp
	@touch utilities.tmp
	for src in $(libutilities_la_SOURCES); do \
	  $(NOTANGLE) -R[[$$src]] $^ | $(CPIF) $$src; \
        done
	@mv -f utilities.tmp utilities.stamp

$(libutilities_la_SOURCES): utilities.stamp
## Recover from the removal of $@
	@if test -f $@; then :; else \
	  rm -f utilities.stamp; \
	  $(MAKE) $(AM_MAKEFLAGS) utilities.stamp; \
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
	-rm -f utilities.stamp utilities.tmp
	-rm -f *.$(FC_MODULE_EXT)

## Remove backup files
maintainer-clean-backup:
	-rm -f *~
.PHONY: maintainer-clean-backup

## Register additional clean targets
maintainer-clean-local: maintainer-clean-noweb maintainer-clean-backup
