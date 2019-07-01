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

## Subdirectories to configure
SUBDIRS = \
  noweb-frame \
  basics \
  utilities \
  testing \
  system \
  combinatorics parsing rng expr_base \
  sindarin \
  physics qft \
  fastjet \
  types \
  matrix_elements \
  hepmc lcio particles \
  lhapdf \
  lhapdf5 \
  pdf_builtin \
  hoppet \
  beams \
  me_methods \
  stdhep xdr events \
  phase_space \
  mci variables \
  muli shower \
  user \
  fks blha \
  gosam openloops \
  looptools \
  model_features \
  process_integration \
  powheg \
  transforms \
  whizard-core \
  prebuilt \
  models
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
  transforms/libtransforms.la \
  ../vamp/src/libvamp.la \
  ../circe1/src/libcirce1.la \
  ../circe2/src/libcirce2.la \
  shower/libshower.la \
  muli/libmuli.la \
  pdf_builtin/libpdf_builtin.la \
  model_features/libmodel_features.la \
  variables/libvariables.la \
  process_integration/libprocess_integration.la \
  powheg/libpowheg.la \
  fks/libfks.la \
  gosam/libgosam.la \
  openloops/liboloops.la \
  blha/libblha.la \
  user/libuser.la \
  mci/libmci.la \
  phase_space/libphase_space.la \
  xdr/libWOStdHep.la \
  events/libevents.la \
  beams/libbeams.la \
  particles/libparticles.la \
  me_methods/libme_methods.la \
  matrix_elements/libmatrix_elements.la \
  types/libtypes.la \
  qft/libqft.la \
  physics/libphysics.la \
  sindarin/libsindarin.la \
  expr_base/libexpr_base.la \
  rng/librng.la \
  parsing/libparsing.la \
  combinatorics/libcombinatorics.la \
  system/libsystem.la \
  testing/libtesting.la \
  utilities/libutilities.la \
  basics/libbasics.la

if HEPMC_AVAILABLE
libwhizard_la_LIBADD += hepmc/libHepMCWrap.la
else
libwhizard_la_LIBADD += hepmc/libHepMCWrap_dummy.la
endif 

if LCIO_AVAILABLE
libwhizard_la_LIBADD += lcio/libLCIOWrap.la
else
libwhizard_la_LIBADD += lcio/libLCIOWrap_dummy.la
endif 

libomega_la_LIBADD = \
  ../omega/src/libomega_core.la \
  models/libmodels.la


## If (parts of) LHAPDF is not available, link in a dummy as replacements
if LHAPDF5_AVAILABLE
libwhizard_la_LIBADD += $(LDFLAGS_LHAPDF)
if LHAPDF5_HAS_PHOTON_DUMMY
libwhizard_la_LIBADD += lhapdf5/libLHAPDF5_dummy.la
endif
else
if !LHAPDF6_AVAILABLE
libwhizard_la_LIBADD += lhapdf5/libLHAPDF5_dummy.la
endif
endif

if LHAPDF6_AVAILABLE
libwhizard_la_LIBADD += $(LHAPDF_LIBS) lhapdf/libLHAPDFWrap.la
else
libwhizard_la_LIBADD += lhapdf/libLHAPDFWrap_dummy.la
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

if FASTJET_AVAILABLE
libwhizard_la_LIBADD += $(FASTJET_LIBS) fastjet/libFastjetWrap.la
else
libwhizard_la_LIBADD += fastjet/libFastjetWrap_dummy.la
endif 

if LOOPTOOLS_AVAILABLE 
libwhizard_la_LIBADD += $(LDFLAGS_LOOPTOOLS) looptools/liblooptools.la 
else 
libwhizard_la_LIBADD += looptools/liblooptools_dummy.la 
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
whizard_LDADD += $(LDFLAGS_LCIO)
whizard_LDADD += $(LDFLAGS_HOPPET)
whizard_LDADD += $(FASTJET_LIBS)
whizard_LDADD += $(LDFLAGS_LOOPTOOLS)

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

## Remove Module lists only after final subdir cleanup
distclean-local:
	for d in $(SUBDIRS); do \
	  rm -f $$d/Modules; \
	done
