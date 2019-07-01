## Makefile.am -- Makefile for WHIZARD
##
## Process this file with automake to produce Makefile.in

## Subdirectories to configure
SUBDIRS = \
  pythia matching hepmc lhapdf stdhep circe1 \
  circe2 misc vamp shower whizard-core prebuilt \
  omega models 
if MPOST_AVAILABLE
SUBDIRS += gamelan feynmf
endif

## -------------------------------------------------------------------
## Build the toplevel WHIZARD library, which wraps various libraries
## built in the subdirectories
## and the wrapper O'Mega library, which contains omegalib and
## the W/O interface modules for the various models
pkglib_LTLIBRARIES = libwhizard.la libomega.la

libwhizard_la_SOURCES =

libomega_la_SOURCES =

## Collect the various partial libraries
libwhizard_la_LIBADD = \
  whizard-core/libwhizard_core.la \
  vamp/src/libvamp.la \
  circe1/src/libcirce1.la \
  circe2/src/libcirce2.la \
  misc/libaux.la
if SHOWER_AVAILABLE
libwhizard_la_LIBADD += \
  shower/libshower.la 
else 
libwhizard_la_LIBADD += \
  shower/libshower_dummy.la
endif 

if HEPMC_AVAILABLE
libwhizard_la_LIBADD += hepmc/libHepMCWrap.la
else
libwhizard_la_LIBADD += hepmc/libHepMCWrap_dummy.la
endif 

libomega_la_LIBADD = \
  omega/src/libomega_core.la \
  models/libmodels.la


## If LHAPDF is not available, link in a dummy as replacements
if LHAPDF_AVAILABLE
libwhizard_la_LIBADD += $(LDFLAGS_LHAPDF)
else
libwhizard_la_LIBADD += lhapdf/libLHAPDF_dummy.la
endif 

## If STDHEP is not available, link in a dummy as replacements
if STDHEP_AVAILABLE
libwhizard_la_LIBADD += $(LDFLAGS_STDHEP)
else
libwhizard_la_LIBADD += stdhep/libstdhep_dummy.la
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

if WHIZARD_WITH_OPENMP
whizard_LDADD += $(OPENMP_FCFLAGS)
libwhizard_la_LIBADD += $(OPENMP_FCFLAGS)
libomega_la_LIBADD += $(OPENMP_FCFLAGS)
endif

## -------------------------------------------------------------------
## Remove backup files
maintainer-clean-local:
	-rm -f *~
