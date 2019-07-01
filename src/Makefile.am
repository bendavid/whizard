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

## Subdirectories to configure
SUBDIRS = \
  pythia hepmc lhapdf stdhep hoppet circe1 circe2 misc pdf_builtin vamp \
  shower muli whizard-core prebuilt omega models 
if MPOST_AVAILABLE
SUBDIRS += gamelan feynmf
endif

## -------------------------------------------------------------------
## Build the toplevel WHIZARD library, which wraps various libraries
## built in the subdirectories
## and the wrapper O'Mega library, which contains omegalib and
## the W/O interface modules for the various models
lib_LTLIBRARIES = libwhizard.la libomega.la

libwhizard_la_SOURCES =
libwhizard_la_LDFLAGS = $(LIBRARY_VERSION)

libomega_la_SOURCES =

## Collect the various partial libraries
libwhizard_la_LIBADD = \
  whizard-core/libwhizard_core.la \
  vamp/src/libvamp.la \
  circe1/src/libcirce1.la \
  circe2/src/libcirce2.la \
  misc/libaux.la \
  pdf_builtin/libpdf_builtin.la
if SHOWER_AVAILABLE
libwhizard_la_LIBADD += \
  shower/libshower.la 
else 
libwhizard_la_LIBADD += \
  shower/libshower_dummy.la
endif 
if MPI_AVAILABLE
libwhizard_la_LIBADD += \
  muli/libmuli.la 
else 
libwhizard_la_LIBADD += \
  muli/libmuli_dummy.la
endif 

if HEPMC_AVAILABLE
libwhizard_la_LIBADD += hepmc/libHepMCWrap.la
else
libwhizard_la_LIBADD += hepmc/libHepMCWrap_dummy.la
endif 

libomega_la_LIBADD = \
  omega/src/libomega_core.la \
  models/libmodels.la


## If (parts of) LHAPDF is not available, link in a dummy as replacements
if LHAPDF_AVAILABLE
libwhizard_la_LIBADD += $(LDFLAGS_LHAPDF)
endif

if LHAPDF_DUMMY
libwhizard_la_LIBADD += lhapdf/libLHAPDF_dummy.la
endif 

## If STDHEP is not available, link in a dummy as replacements
if STDHEP_AVAILABLE
libwhizard_la_LIBADD += $(LDFLAGS_STDHEP)
else
libwhizard_la_LIBADD += stdhep/libstdhep_dummy.la
endif 

if HOPPET_AVAILABLE
libwhizard_la_LIBADD += $(LDFLAGS_HOPPET) hoppet/libhoppet.la
else
libwhizard_la_LIBADD += hoppet/libhoppet_dummy.la
endif 

## -------------------------------------------------------------------
## Build a standalone program
bin_PROGRAMS = whizard 

whizard_SOURCES =

## A dummy source tells libtool that the F90 compiler is used for linking
## Without dummy, libtool uses the C linker (default: ld)
nodist_EXTRA_whizard_SOURCES = dummy.f90

whizard_LDADD  = whizard-core/libwhizard_main.la
whizard_LDADD += ./libwhizard.la
whizard_LDADD += prebuilt/libwhizard_prebuilt.la
whizard_LDADD += $(LDFLAGS_LHAPDF)
whizard_LDADD += $(LDFLAGS_STDHEP)
whizard_LDADD += $(LDFLAGS_HEPMC)
whizard_LDADD += $(LDFLAGS_HOPPET)

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
## Non-standard cleanup tasks

## Remove backup files
maintainer-clean-local:
	-rm -f *~
