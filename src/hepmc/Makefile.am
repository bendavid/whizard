## Makefile.am -- Makefile for WHIZARD
##
## Process this file with automake to produce Makefile.in

## The files in this directory end up in an auxiliary libtool library.
if HEPMC_AVAILABLE
noinst_LTLIBRARIES = libHepMCWrap.la
libHepMCWrap_la_SOURCES = HepMCWrap.cpp
libHepMCWrap_la_CPPFLAGS = $(HEPMC_INCLUDES)
else
noinst_LTLIBRARIES = libHepMCWrap_dummy.la
libHepMCWrap_dummy_la_SOURCES = HepMCWrap_dummy.f90
endif

## OpenMP
AM_FCFLAGS =
AM_LDFLAGS =
if WHIZARD_WITH_OPENMP
AM_FCFLAGS += $(OPENMP_FCFLAGS)
AM_LDFLAGS += $(OPENMP_FCFLAGS)
endif

## Remove backup files
maintainer-clean-local:
	-rm -f *~
