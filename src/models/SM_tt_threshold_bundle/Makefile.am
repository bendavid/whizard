## Makefile.am -- Makefile for model-parameter modules in WHIZARD
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
#     Felix Braam, Sebastian Schmidt
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

## Build the objects required for the SM ttbar threshold model.

noinst_LTLIBRARIES = libSM_tt_threshold_bundle.la

## We need all in the distribution

EXTRA_DIST = interpolation.f90  nr_tools.f90  \
	toppik.f  toppik_axial.f  ttv_formfactors.f90

libSM_tt_threshold_bundle_la_SOURCES = \
  interpolation.f90 \
  nr_tools.f90 \
  toppik.f \
  toppik_axial.f \
  ttv_formfactors.f90

# Dump module names into file Modules
libSM_tt_threshold_bundle_Modules = \
	${libSM_tt_threshold_bundle_la_SOURCES:.f90=}
Modules: Makefile
	@rm -f $@
	echo SM_tt_threshold > $@
	for module in $(libSM_tt_threshold_bundle_Modules); do \
	  echo $$module >> $@; \
	done

## Fortran module dependencies
# Get module lists from other directories
module_lists = \
  ../../basics/Modules \
  ../../utilities/Modules \
  ../../system/Modules \
  ../../physics/Modules \
  ../../combinatorics/Modules

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
Makefile.depend: $(libSM_tt_threshold_bundle_la_SOURCES)
	@rm -f $@
	for src in $^; do \
	  module="`basename $$src | sed 's/\.f[90][0358]//'`"; \
	  grep '^ *use ' $$src \
	    | grep -v '!NODEP!' \
	    | sed -e 's/^ *use */'$$module'.lo: /' \
	          -f Module_dependencies.sed; \
	done > $@

DISTCLEANFILES += Makefile.depend

## Install the modules used by generated matrix element code
execmoddir = $(pkglibdir)/mod/models
nodist_execmod_HEADERS = \
	interpolation.$(FC_MODULE_EXT) \
	nr_tools.$(FC_MODULE_EXT) \
	ttv_formfactors.$(FC_MODULE_EXT)

SUFFIXES = .lo .$(FC_MODULE_EXT)

# Fortran90 module files are generated at the same time as object files
.lo.$(FC_MODULE_EXT):
	@:
#       touch $@

AM_FCFLAGS = -I../../basics -I../../testing -I../../utilities -I../../system -I../../physics -I../../combinatorics

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
## Non-standard cleanup tasks
clean-local:
	-rm -f *.$(FC_MODULE_EXT)
if FC_SUBMODULES
	-rm -f *.smod
endif

## Remove backup files
maintainer-clean-local:
	-rm -f *~
