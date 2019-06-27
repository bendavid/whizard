# Makefile.am --
# $Id: Makefile.am 319 2010-04-18 15:00:54Z ohl $
########################################################################

## include the iso_fortran_env stub if required and make sure that it is
## distributed in any case.
if ISO_FORTRAN_ENV_STUB
sources_extra = iso_fortran_env_stub.f90
depend_filter_extra = -e 's/iso_fortran_env/iso_fortran_env_stub/'
modules_extra = iso_fortran_env.mod
iso_fortran_env.mod: iso_fortran_env_stub.lo
else
depend_filter_extra = -e 's/iso_fortran_env\.lo//'
endif

SOURCE_FILES = \
  divisions.f90 \
  vamp.f90 \
  kinds.f90 \
  exceptions.f90 \
  tao_random_numbers.f90 \
  tao52_random_numbers.f90 \
  specfun.f90 \
  vamp_stat.f90 \
  histograms.f90 \
  utils.f90 \
  linalg.f90 \
  products.f90 \
  $(sources_extra)

MODULE_FILES = \
  divisions.$(FC_MODULE_EXT) \
  vamp.$(FC_MODULE_EXT) \
  kinds.$(FC_MODULE_EXT) \
  exceptions.$(FC_MODULE_EXT) \
  tao_random_numbers.$(FC_MODULE_EXT) \
  tao52_random_numbers.$(FC_MODULE_EXT) \
  specfun.$(FC_MODULE_EXT) \
  vamp_stat.$(FC_MODULE_EXT) \
  histograms.$(FC_MODULE_EXT) \
  utils.$(FC_MODULE_EXT) \
  linalg.$(FC_MODULE_EXT) \
  products.$(FC_MODULE_EXT) \
  $(modules_extra) \
  # vamp_grid.$(FC_MODULE_EXT) \
  # vamp_equivalences.$(FC_MODULE_EXT)

NOWEB_FILES = \
    prelude.nw postlude.nw pmep.nw \
    divisions.nw vamp.nw \
    vamp_kinds.nw constants.nw exceptions.nw \
    tao_random_numbers.nw \
    specfun.nw vamp_stat.nw histograms.nw utils.nw \
    linalg.nw products.nw kinematics.nw coordinates.nw \
    vampi.nw mpi90.nw vamp_test.nw vamp_test0.nw \
    application.nw sample.nw

if MPI
pkglib_LTLIBRARIES = libvamp.la libvampi.la
libvamp_la_SOURCES = $(SOURCE_FILES)
libvampi_la_SOURCES = mpi90.f90 vampi.f90
pkginclude_DATA = $(MODULE_FILES) # mpi90.$(FC_MODULE_EXT) vampi.$(FC_MODULE_EXT)
else !MPI
pkglib_LTLIBRARIES = libvamp.la
libvamp_la_SOURCES = $(SOURCE_FILES)
pkginclude_DATA = $(MODULE_FILES)
endif !MPI

EXTRA_DIST = $(NOWEB_FILES) $(SOURCE_FILES) f90.m4 f95.m4 iso_fortran_env_stub.f90

# Fortran90 module files are generated at the same time as object files
.lo.$(FC_MODULE_EXT):
	@:
#	touch $@

########################################################################
# Fortran Compilers
########################################################################

FORTRAN_DIALECT = @FORTRAN_DIALECT@

########################################################################
# noweb
########################################################################

NOTANGLE_IT = \
  $(NOTANGLE) -R'[[$(@F)]]' $(srcdir)/prelude.nw $(srcdir)/pmep.nw $^ \
    | $(M4) $(FORTRAN_DIALECT).m4 - | $(CPIF) $@

SUFFIXES = .nw 

$(SOURCE_FILES): $(FORTRAN_DIALECT).m4

.nw.f90:
	$(NOTANGLE_IT)

tao52_random_numbers.f90: tao_random_numbers.nw
	$(NOTANGLE_IT)

f90.m4: pmep.nw
	$(NOTANGLE) -R'[[$(@F)]]' $(srcdir)/pmep.nw | $(CPIF) $@

f95.m4: pmep.nw
	$(NOTANGLE) -R'[[$(@F)]]' $(srcdir)/pmep.nw | $(CPIF) $@

########################################################################
# The following line just says
#    include Makefile.depend
# but in a portable fashion (depending on automake's AM_MAKE_INCLUDE
########################################################################
@am__include@ @am__quote@Makefile.depend@am__quote@

Makefile.depend: $(SOURCE_FILES)
	@rm -f $@
	for src in $^; do \
	  module="`basename $$src | sed 's/\.f[90][0358]//'`"; \
	  grep '^ *use ' $$src \
	    | grep -v '!NODEP!' \
	    | sed -e 's/^ *use */'$$module'.lo: /' \
	          -e 's/, *only:.*//' \
	          -e 's/, *&//' \
	          -e 's/, *.*=>.*//' \
	          -e 's/ *$$/.lo/' \
				 $(depend_filter_extra) ; \
	done > $@

DISTCLEANFILES = Makefile.depend

clean-local:
	rm -f *.$(FC_MODULE_EXT) *.g90

########################################################################
# MPI
########################################################################
###
### # The -mismatch_all is for mpi_send() etc.
### MPIFC = mpif90
### MPIFCFLAGS = # -mismatch_all
