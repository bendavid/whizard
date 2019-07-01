## Makefile.am -- Makefile for WHIZARD
# $Id: Makefile.am 1564 2010-01-21 18:19:23Z ohl $
##
## Process this file with automake to produce Makefile.in
##
########################################################################
#
# Copyright (C) 1999-2016 by 
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

## The files in this directory end up in an auxiliary libtool library.
noinst_LTLIBRARIES = libbasics.la

### WK 15.09.2014: this stub module is probably obsolete, to be deleted:
# ## include the iso_fortran_env stub if required and make sure that it is
# ## distributed in any case.
# if ISO_FORTRAN_ENV_STUB
# sources_extra = iso_fortran_env_stub.f90
# modules_extra = iso_fortran_env.$(FC_MODULE_EXT)
# iso_fortran_env_depend = iso_fortran_env_stub.lo
# iso_fortran_env.mod: iso_fortran_env_stub.lo
# endif
# EXTRA_DIST = iso_fortran_env_stub.f90

nodist_libbasics_la_SOURCES = \
  kinds.f90
libbasics_la_SOURCES = \
  iso_varying_string.f90 \
  io_units.f90 \
  constants.f90

# Dump module names into file Modules
libbasics_Modules = ${nodist_libbasics_la_SOURCES:.f90=} ${libbasics_la_SOURCES:.f90=} 
Modules: Makefile
	@for module in $(libbasics_Modules); do \
          echo $$module >> $@.new; \
        done
	@if diff $@ $@.new -q >/dev/null; then \
          rm $@.new; \
        else \
          mv $@.new $@; echo "Modules updated"; \
        fi
BUILT_SOURCES = Modules


## Install the modules used by generated matrix element code
execmoddir = $(pkglibdir)/mod/basics
nodist_execmod_HEADERS = \
  kinds.$(FC_MODULE_EXT) \
  iso_varying_string.$(FC_MODULE_EXT) \
  io_units.$(FC_MODULE_EXT) \
  constants.$(FC_MODULE_EXT)

SUFFIXES = .lo .$(FC_MODULE_EXT)
# Fortran90 module files are generated at the same time as object files
.lo.$(FC_MODULE_EXT):
	@:
#	touch $@

# Explicit dependencies
constants.lo: kinds.lo


AM_FCFLAGS =

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

.PHONY: clean-noweb

clean-local: clean-noweb
	-rm -f *.$(FC_MODULE_EXT)
if FC_SUBMODULES
	-rm -f *.smod
endif

DISTCLEANFILES = kinds.f90

## Remove backup files
maintainer-clean-local:
	-rm -f *~
