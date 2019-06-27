## Makefile.am -- Makefile for WHIZARD - SHOWER
##
## Process this file with automake to produce Makefile.in

## The files in this directory end up in an auxiliary libtool library.
noinst_LTLIBRARIES = libshower.la

if PYTHIA_AVAILABLE
if FC_IS_NAG
AM_FFLAGS = -dcfuns -w
endif
libshower_la_SOURCES = pythia_up.f ../matching/pythia.f shower_basics_module.f90 shower_parton_module.f90 shower_module.f90
else
libshower_la_SOURCES = pythia_dummy.f90 shower_basics_module.f90 shower_parton_module.f90 shower_module.f90
endif

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

AM_FCFLAGS = -I../misc -I../vamp/src/

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
