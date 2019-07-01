## Makefile.am -- Makefile for WHIZARD
##
## Process this file with automake to produce Makefile.in
#
# Copyright (C) 1999-2015 by 
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
if FASTJET_AVAILABLE
noinst_LTLIBRARIES = libFastjetWrap.la
libFastjetWrap_la_SOURCES = \
  CppStringsWrap.cpp FastjetWrap.cpp \
  cpp_strings.f90 fastjet.f90
libFastjetWrap_la_CPPFLAGS = $(FASTJET_CXXFLAGS)
else
noinst_LTLIBRARIES = libFastjetWrap_dummy.la
libFastjetWrap_dummy_la_SOURCES = \
  CppStringsWrap_dummy.f90 FastjetWrap_dummy.f90 \
  cpp_strings.f90 fastjet.f90
endif

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
## Explicit dependencies

fastjet.lo: cpp_strings.lo
fastjet.lo: \
  ../basics/kinds.$(FC_MODULE_EXT) \
  ../physics/lorentz.$(FC_MODULE_EXT)

AM_FCFLAGS += -I../basics -I../utilities -I../system -I../physics

MODULES= \
  cpp_strings.$(FC_MODULE_EXT) \
  fastjet.$(FC_MODULE_EXT)

########################################################################
## Non-standard cleanup tasks

## Remove backup files
maintainer-clean-local:
	-rm -f *~

## Remove module files
clean-local:
	-rm -f $(MODULES)
