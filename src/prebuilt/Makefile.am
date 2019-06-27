## Makefile.am -- Makefile for WHIZARD prebuilt process libraries
##
## Process this file with automake to produce Makefile.in

## The main program makes up a library on its own.
pkglib_LTLIBRARIES = libwhizard_prebuilt.la

libwhizard_prebuilt_la_SOURCES = libmanager.f90

## Dependencies of whizard_prebuilt
$(libwhizard_prebuilt_la_OBJECTS): \
  ../misc/iso_varying_string.$(FC_MODULE_EXT) \
  ../whizard-core/process_libraries.$(FC_MODULE_EXT)

AM_FCFLAGS = -I../misc -I../whizard-core

## OpenMP
AM_LDFLAGS =
if WHIZARD_WITH_OPENMP
AM_FCFLAGS += $(OPENMP_FCFLAGS)
AM_LDFLAGS += $(OPENMP_FCFLAGS)
endif

## Remove F90 module files
clean-local:
	-rm -f *.$(FC_MODULE_EXT)

## Remove backup files
maintainer-clean-backup:
	-rm -f *~
.PHONY: maintainer-clean-backup

## Register additional clean targets
maintainer-clean-local: maintainer-clean-backup
