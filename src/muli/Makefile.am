## Makefile.am -- Makefile for WHIZARD - Multiple Interactions
##
## Process this file with automake to produce Makefile.in

## The files in this directory end up in an auxiliary libtool library.

if MPI_AVAILABLE
noinst_LTLIBRARIES = libmuli.la
else
noinst_LTLIBRARIES = libmuli_dummy.la
endif

# commented out by SS
#AM_FFLAGS = @FC_PROF@
AM_FFLAGS = ""
if FC_IS_NAG
AM_FFLAGS += -dcfuns -w
endif
libmuli_la_SOURCES = muli.f90 muli_basic.f90 muli_cuba.f90 muli_momentum.f90 \
   muli_interactions.f90 muli_trapezium.f90 muli_fibonacci_tree.f90 \
   muli_aq.f90 muli_dsigma.f90 muli_remnant.f90 muli_mcint.f90
libmuli_dummy_la_SOURCES = muli_dummy.f90

## Fortran module dependencies
# The following line just says
#    include Makefile.depend
# but in a portable fashion (depending on automake's AM_MAKE_INCLUDE
@am__include@ @am__quote@Makefile.depend@am__quote@

Makefile.depend: $(libmuli_la_SOURCES)
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

# commented out by SS
#AM_FCFLAGS = @FC_PROF@ -I../misc -I../vamp/src/
AM_FCFLAGS = -I../basics -I../misc -I../system -I../../vamp/src/ -I../pdf_builtin -I../utilities

# commented out by SS
### OpenMP
#AM_LDFLAGS =
#if WHIZARD_WITH_OPENMP
#AM_FCFLAGS += $(OPENMP_FCFLAGS)
#AM_LDFLAGS += $(OPENMP_FCFLAGS)
#endif

## Non-standard cleanup tasks
clean-local:
	-rm -f *.$(FC_MODULE_EXT)

## Remove backup files
maintainer-clean-local:
	-rm -f *~
