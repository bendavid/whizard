## Makefile.am -- Makefile for WHIZARD - SHOWER
##
## Process this file with automake to produce Makefile.in

## The files in this directory end up in an auxiliary libtool library.
if SHOWER_AVAILABLE
noinst_LTLIBRARIES = libshower.la
else
noinst_LTLIBRARIES = libshower_dummy.la
endif

## If shower is enabled, pythia is enabled automatically

AM_FFLAGS =
AM_FCFLAGS =

if FC_IS_NAG
AM_FFLAGS += -dcfuns -w
AM_FCFLAGS += -dcfuns -w
endif
libshower_la_SOURCES = pythia_up.f shower_basics_module.f90 shower_parton_module.f90 shower_module.f90 shower_topythia.f90 mlm_matching_module.f90 ckkw_pseudo_weights_module.f90 ckkw_matching_module.f90
libshower_dummy_la_SOURCES = shower_dummy.f90

## Fortran module dependencies
# The following line just says
#    include Makefile.depend
# but in a portable fashion (depending on automake's AM_MAKE_INCLUDE
@am__include@ @am__quote@Makefile.depend@am__quote@

Makefile.depend: $(libshower_la_SOURCES)
	@rm -f $@
	for src in $^; do \
	  module="`basename $$src | sed 's/\.f[90][0358]//'`"; \
	  grep '^ *use ' $$src \
	    | grep -v '!NODEP!' \
	    | sed -e 's/^ *use */'$$module'.lo: /' \
	          -e 's/, *only:.*//' \
	          -e 's/, *&//' \
	          -e 's/, *.*=>.*//' \
	          -e 's/$$/.lo/' ; \
	done > $@

DISTCLEANFILES = Makefile.depend 

## Dependency libaux (kinds, strings)
$(libvamp_la_OBJECTS): ../misc/kinds.$(FC_MODULE_EXT)

libshower_la_LIBADD = ../pythia/libpythia_wo.la

AM_FCFLAGS += -I../misc -I../vamp/src/ -I../pythia

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
clean-local:
	-rm -f *.$(FC_MODULE_EXT)

## Remove backup files
maintainer-clean-local:
	-rm -f *~
