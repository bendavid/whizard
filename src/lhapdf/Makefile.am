## Makefile.am -- Makefile for WHIZARD
##
## Process this file with automake to produce Makefile.in

## This part is only necessary if LHAPDF is unavailable.  We create
## a dummy object (library) that resolves otherwise undefined symbols.
if !LHAPDF_AVAILABLE

noinst_LTLIBRARIES = libLHAPDF_dummy.la

libLHAPDF_dummy_la_SOURCES = lhapdf_dummy.f90

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
