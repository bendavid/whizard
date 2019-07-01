## Makefile.am -- Makefile for WHIZARD
# $Id: Makefile.am 1564 2010-01-21 18:19:23Z ohl $
##
## Process this file with automake to produce Makefile.in
##
########################################################################
#
# Copyright (C) 1999-2014 by 
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

## The files in this directory end up in an auxiliary libtool library.
noinst_LTLIBRARIES = libaux.la

## include the iso_fortran_env stub if required and make sure that it is
## distributed in any case.
if ISO_FORTRAN_ENV_STUB
sources_extra = iso_fortran_env_stub.f90
modules_extra = iso_fortran_env.$(FC_MODULE_EXT)
iso_fortran_env_depend = iso_fortran_env_stub.lo
iso_fortran_env.mod: iso_fortran_env_stub.lo
endif
EXTRA_DIST = iso_fortran_env_stub.f90

libaux_la_SOURCES = \
  kinds.f90 iso_varying_string.f90 \
  system_dependencies.f90 \
  constants.f90 \
  limits.f90 \
  file_utils.f90 \
  diagnostics.f90 signal_interface.c \
  c_particles.f90 lorentz.f90 \
  sm_physics.f90 \
  $(sources_extra)

WHIZARD_SRC_FROM_WHIZARD_NW = \
  limits.f90 \
  file_utils.f90 \
  diagnostics.f90 signal_interface.c \
  c_particles.f90 lorentz.f90 sm_physics.f90

## Explicit dependencies:
limits.lo: kinds.lo $(iso_fortran_env_depend)
constants.lo: kinds.lo
file_utils.lo: kinds.lo iso_varying_string.lo limits.lo \
  $(iso_fortran_env_depend)
diagnostics.lo: kinds.lo iso_varying_string.lo system_dependencies.lo \
  file_utils.lo limits.lo $(iso_fortran_env_depend)
c_particles.lo: file_utils.lo
lorentz.lo: kinds.lo file_utils.lo constants.lo diagnostics.lo c_particles.lo
sm_physics.lo: kinds.lo file_utils.lo constants.lo lorentz.lo diagnostics.lo

## Install the modules used by generated matrix element code
execmoddir = $(pkglibdir)/mod/misc
nodist_execmod_HEADERS = \
  kinds.$(FC_MODULE_EXT) \
  constants.$(FC_MODULE_EXT) \
  iso_varying_string.$(FC_MODULE_EXT) \
  system_dependencies.$(FC_MODULE_EXT) \
  limits.$(FC_MODULE_EXT) \
  file_utils.$(FC_MODULE_EXT) \
  diagnostics.$(FC_MODULE_EXT) \
  c_particles.$(FC_MODULE_EXT) \
  lorentz.$(FC_MODULE_EXT) \
  sm_physics.$(FC_MODULE_EXT) \
  $(modules_extra)

SUFFIXES = .lo .$(FC_MODULE_EXT)
# Fortran90 module files are generated at the same time as object files
.lo.$(FC_MODULE_EXT):
	@:
#	touch $@

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

## (Re)create F90 sources from NOWEB source.
if NOWEB_AVAILABLE

whizard.stamp: $(top_srcdir)/src/whizard-core/whizard.nw
	@rm -f whizard.tmp
	@touch whizard.tmp
	for src in $(WHIZARD_SRC_FROM_WHIZARD_NW); do \
	  $(NOTANGLE) -R[[$$src]] $< | $(CPIF) $$src; \
        done
	@mv -f whizard.tmp whizard.stamp

$(WHIZARD_SRC_FROM_WHIZARD_NW): whizard.stamp
## Recover from the removal of $@
	@if test -f $@; then :; else \
	  rm -f whizard.stamp; \
	  $(MAKE) $(AM_MAKEFLAGS) whizard.stamp; \
	fi

endif


## Non-standard cleanup tasks
if NOWEB_AVAILABLE
clean-noweb:
	-rm -f whizard.stamp whizard.tmp
	test "$(srcdir)" != "." && rm -f $(WHIZARD_SRC_FROM_WHIZARD_NW) || true
endif
.PHONY: clean-noweb

clean-local: clean-noweb
	-rm -f *.$(FC_MODULE_EXT)

## Remove backup files
maintainer-clean-local:
	-rm -f *~
