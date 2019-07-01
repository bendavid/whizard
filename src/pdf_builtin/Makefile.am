## Makefile.am -- Makefile for WHIZARD
# $Id: Makefile.am 6465 2015-01-10 15:22:31Z jr_reuter $
##
## Process this file with automake to produce Makefile.in
##
########################################################################
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

noinst_LTLIBRARIES = libpdf_builtin.la

libpdf_builtin_la_SOURCES = \
	cteq6pdf.f pdf_builtin.f90 mrst2004qed.f mstwpdf.f \
	ct10pdf.f CJpdf.f

$(libpdf_builtin_la_OBJECTS): \
	../basics/kinds.$(FC_MODULE_EXT) \
	../basics/iso_varying_string.$(FC_MODULE_EXT) \
	../basics/io_units.$(FC_MODULE_EXT) \
	../utilities/format_utils.$(FC_MODULE_EXT) \
	../system/diagnostics.$(FC_MODULE_EXT)

AM_FFLAGS = -I../basics -I../utilities -I../system
AM_FCFLAGS = -I../basics -I../utilities -I../system

# After hacking, those .f files are actually fixed-form F90 -> make sure they
# get compiled with the appropiate compiler.
F77 = $(FC)

pdf_builtin.lo: cteq6pdf.lo mrst2004qed.lo mstwpdf.lo ct10pdf.lo CJpdf.lo

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
## Non-standard cleanup tasks

## Remove F90 module files
clean-local:
	-rm -f *.$(FC_MODULE_EXT)

## Remove backup files
maintainer-clean-local:
	-rm -f *~
