## Makefile.am -- Makefile for WHIZARD
##
## Process this file with automake to produce Makefile.in
#
# Copyright (C) 1999-2016 by 
#     Wolfgang Kilian <kilian@physik.uni-siegen.de>
#     Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
#     Juergen Reuter <juergen.reuter@desy.de>
#     with contributions from
#     Fabian Bach <fabian.bach@t-online.de>
#     Bijan Chokoufe <bijan.chokoufe@desy.de>
#     Christian Speckner <cnspeckn@googlemail.com>
#     Marco Sekulla <marco.sekulla@kit.edu>
#     Christian Weiss <christian.weiss@desy.de>
#     Felix Braam, Sebastian Schmidt
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
  xdr events \
  phase_space \
  mci variables \
  pythia8 \
  tauola \
  muli shower \
  user \
  threshold \
  blha \
  gosam openloops \
  looptools \
  model_features \
  fks \
  process_integration \
  matching \
  transforms \
  whizard-core \
  prebuilt \
  models

if MPOST_AVAILABLE
SUBDIRS += gamelan feynmf
endif

########################################################################
## Build 
## (1) the toplevel WHIZARD library, which wraps various libraries
## built in the subdirectories
## (2) the WHIZARD test library that is linked to the main executable
## as the provider of internal unit tests
## (3) the wrapper O'Mega library, which contains omegalib and
## the W/O interface modules for the various models

lib_LTLIBRARIES = libwhizard.la libomega.la

libwhizard_la_SOURCES =
libwhizard_la_LDFLAGS = $(LIBRARY_VERSION)

## Collect the various partial libraries
libwhizard_la_LIBADD = \
  threshold/libthreshold.la \
  whizard-core/libwhizard_core.la \
  transforms/libtransforms.la \
  ../vamp/src/libvamp.la \
  ../circe1/src/libcirce1.la \
  ../circe2/src/libcirce2.la \
  shower/libshower.la \
  tauola/libtauola_interface.la \
  muli/libmuli.la \
  pdf_builtin/libpdf_builtin.la \
  model_features/libmodel_features.la \
  variables/libvariables.la \
  process_integration/libprocess_integration.la \
  matching/libmatching.la \
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
## WHIZARD unit-test library

check_LTLIBRARIES = libwhizard_test.la

libwhizard_test_la_SOURCES =

libwhizard_test_la_LIBADD = \
  threshold/libthreshold_ut.la \
  whizard-core/libwhizard_core_ut.la \
  transforms/libtransforms_ut.la \
  matching/libmatching_ut.la \
  fks/libfks_ut.la \
  process_integration/libprocess_integration_ut.la \
  blha/libblha_ut.la \
  model_features/libmodel_features_ut.la \
  mci/libmci_ut.la \
  phase_space/libphase_space_ut.la \
  events/libevents_ut.la \
  beams/libbeams_ut.la \
  particles/libparticles_ut.la \
  me_methods/libme_methods_ut.la \
  matrix_elements/libmatrix_elements_ut.la \
  types/libtypes_ut.la \
  qft/libqft_ut.la \
  physics/libphysics_ut.la \
  sindarin/libsindarin_ut.la \
  rng/librng_ut.la \
  parsing/libparsing_ut.la \
  combinatorics/libcombinatorics_ut.la \
  system/libsystem_ut.la

## -------------------------------------------------------------------
## O'Mega main library

libomega_la_SOURCES =

libomega_la_LIBADD = \
  ../omega/src/libomega_core.la \
  models/libmodels.la


########################################################################
## Build a standalone program

bin_PROGRAMS = whizard 

whizard_SOURCES =

## A dummy source tells libtool that the F90 compiler is used for linking
## Without dummy, libtool uses the C linker (default: ld)
nodist_EXTRA_whizard_SOURCES = dummy.f90

whizard_LDADD  = whizard-core/libwhizard_main.la
whizard_LDADD += ./libwhizard.la
whizard_LDADD += prebuilt/libwhizard_prebuilt.la
whizard_LDADD += $(CXXLIBS)
whizard_LDADD += $(LDFLAGS_LHAPDF)
whizard_LDADD += $(LDFLAGS_HEPMC)
whizard_LDADD += $(LDFLAGS_LCIO)
whizard_LDADD += $(LDFLAGS_HOPPET)
whizard_LDADD += $(FASTJET_LIBS)
whizard_LDADD += $(LDFLAGS_LOOPTOOLS)

## -------------------------------------------------------------------
## Build a standalone program for running the unit tests

check_PROGRAMS = whizard_ut

whizard_ut_SOURCES =

## A dummy source tells libtool that the F90 compiler is used for linking
## Without dummy, libtool uses the C linker (default: ld)
nodist_EXTRA_whizard_ut_SOURCES = dummy.f90

whizard_ut_LDADD  = whizard-core/libwhizard_main_ut.la
whizard_ut_LDADD += ./libwhizard_test.la
whizard_ut_LDADD += ./libwhizard.la
whizard_ut_LDADD += prebuilt/libwhizard_prebuilt.la
whizard_ut_LDADD += $(CXXLIBS)
whizard_ut_LDADD += $(LDFLAGS_LHAPDF)
whizard_ut_LDADD += $(LDFLAGS_HEPMC)
whizard_ut_LDADD += $(LDFLAGS_LCIO)
whizard_ut_LDADD += $(LDFLAGS_HOPPET)
whizard_ut_LDADD += $(FASTJET_LIBS)
whizard_ut_LDADD += $(LDFLAGS_LOOPTOOLS)

########################################################################
## Default Fortran compiler options

AM_FCFLAGS =

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
