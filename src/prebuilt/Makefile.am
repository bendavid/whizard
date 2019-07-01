## Makefile.am -- Makefile for WHIZARD prebuilt process libraries
##
## Process this file with automake to produce Makefile.in

## The main program makes up a library on its own.
lib_LTLIBRARIES = libwhizard_prebuilt.la

libwhizard_prebuilt_la_SOURCES = libmanager.f90

## Dependencies of whizard_prebuilt
$(libwhizard_prebuilt_la_OBJECTS): \
  ../basics/iso_varying_string.$(FC_MODULE_EXT) \
  ../matrix_elements/prclib_interfaces.$(FC_MODULE_EXT)

AM_FCFLAGS = -I../basics -I../testing -I../system -I../matrix_elements

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
## Cleanup

## Remove F90 module files
clean-local:
	-rm -f *.$(FC_MODULE_EXT)

## Remove backup files
maintainer-clean-backup:
	-rm -f *~
.PHONY: maintainer-clean-backup

## Register additional clean targets
maintainer-clean-local: maintainer-clean-backup
