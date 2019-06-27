## Makefile.am -- Makefile for WHIZARD
##
## Process this file with automake to produce Makefile.in

## The files in this directory end up in an auxiliary libtool library.
noinst_LTLIBRARIES = libvamp.la

libvamp_la_SOURCES = vamp_bundle.f90

## Dependency libaux (kinds, strings)
$(libvamp_la_OBJECTS): ../misc/kinds.$(FC_MODULE_EXT)

AM_FCFLAGS = -I../misc

## OpenMP
AM_LDFLAGS =
if WHIZARD_WITH_OPENMP
AM_FCFLAGS += $(OPENMP_FCFLAGS)
AM_LDFLAGS += $(OPENMP_FCFLAGS)
endif

## Non-standard cleanup tasks
clean-local:
	-rm -f *.$(FC_MODULE_EXT)

## Remove backup files
maintainer-clean-local:
	-rm -f *~
