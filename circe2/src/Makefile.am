# Makefile.am --
# $Id: Makefile.am 319 2010-04-18 15:00:54Z ohl $
##
## Process this file with automake to produce Makefile.in
##
########################################################################
#
# Copyright (C) 1999-2014 by 
#     Wolfgang Kilian <kilian@physik.uni-siegen.de>
#     Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
#     Juergen Reuter <juergen.reuter@desy.de>
#     with contributions from
#     Christian Speckner <cnspeckn@googlemail.com>
#
# WHIZARD is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by 
# the Free Software Foundation; either version 2, or (at your option)
# any later version.
#
# WHIZARD is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the 
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
#
########################################################################

lib_LTLIBRARIES =
SOURCE_FILES =
MODULE_FILES =
EXTRA_SOURCE_FILES =
NOWEB_FILES = prelude.nw postlude.nw

########################################################################
# The CIRCE2 library proper
########################################################################

lib_LTLIBRARIES += libcirce2.la
libcirce2_la_SOURCES = circe2.f90
MODULE_FILES += circe2.$(FC_MODULE_EXT)
SOURCE_FILES += $(libcirce2_la_SOURCES)
NOWEB_FILES += circe2.nw

########################################################################
# Required for standalone compilation,
# otherwise provided by VAMP and/or WHIZARD
########################################################################

EXTRA_SOURCE_FILES += kinds.f90.in
MODULE_FILES += kinds.$(FC_MODULE_EXT)

lib_LTLIBRARIES += libtaorng.la
libtaorng_la_SOURCES = tao_random_numbers.f90
MODULE_FILES += tao_random_numbers.$(FC_MODULE_EXT)
SOURCE_FILES += $(libtaorng_la_SOURCES)
NOWEB_FILES += circe2_moments.nw

########################################################################
# Testing and tools
########################################################################

bin_PROGRAMS = circe2_moments circe2_ls
circe2_moments_SOURCES = circe2_moments.f90
circe2_moments_LDADD = libcirce2.la libtaorng.la
circe2_moments.o: $(MODULE_FILES)

circe2_ls_SOURCES = circe2_ls.f90
circe2_ls_LDADD = libcirce2.la libtaorng.la
circe2_ls.o: $(MODULE_FILES)

bin_SCRIPTS =

if OCAML_AVAILABLE
bin_SCRIPTS += $(CIRCE2_NATIVE)
endif OCAML_AVAILABLE

$(CIRCE2_NATIVE): $(CIRCE2_CMX)
$(CIRCE2_BYTECODE): $(CIRCE2_CMO)

if OCAML_AVAILABLE
all-local: $(CIRCE2_CMX) $(CIRCE2TOOL_CMX)
bytecode: $(CIRCE2_CMO) $(CIRCE2TOOL_CMO)
else
all-local:
bytecode:
endif

include $(top_srcdir)/src/Makefile.ocaml
include $(top_srcdir)/src/Makefile.sources

EXTRA_DIST = $(NOWEB_FILES) $(SOURCE_FILES) $(CIRCE2_CAML) \
	$(EXTRA_SOURCE_FILES)

MYPRECIOUS = $(CIRCE2_DERIVED)

# Fortran90 module files are generated at the same time as object files
.lo.$(FC_MODULE_EXT):
	@:
#	touch $@

# There are no module files for the FORTRAN 77 code
moduleexecincludedir = $(pkgincludedir)
nodist_moduleexecinclude_HEADERS = $(MODULE_FILES)

AM_FFLAGS = 
AM_FCFLAGS = 

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
# noweb
########################################################################

TRIPLE = $(srcdir)/prelude.nw $< $(srcdir)/postlude.nw
WEBS = $(srcdir)/prelude.nw $(srcdir)/circe2.nw $(srcdir)/postlude.nw
NOTANGLE_IT = \
  cat $(TRIPLE) | $(NOTANGLE) -R'[[$@]]' | $(CPIF) $@

SUFFIXES += .nw .$(FC_MODULE_EXT)

if NOWEB_AVAILABLE
.nw.f90:
	$(NOTANGLE_IT)

circe2_ls.f90: circe2.nw
	$(NOTANGLE_IT)

taorng.f90: postlude.nw
	cat $(WEBS) | $(NOTANGLE) -R'[[$@]]' | $(CPIF) $@

endif NOWEB_AVAILABLE

########################################################################
# O'Caml
########################################################################

if OCAML_AVAILABLE
circe2.top: $(CIRCE2_CMO)
	$(OCAMLMKTOP) $(OCAMLFLAGS) -o $@ \
		unix.cma bigarray.cma $(CIRCE2_CMO)
endif OCAML_AVAILABLE

lexer.mli: lexer.ml parser.cmi
	$(OCAMLC) -i $< | $(GREP) 'val token' >$@


########################################################################
# The following line just says
#    include Makefile.depend
# but in a portable fashion (depending on automake's AM_MAKE_INCLUDE
########################################################################
@am__include@ @am__quote@Makefile.depend@am__quote@

Makefile.depend: $(SOURCE_FILES) $(circe2_moments_SOURCES) $(circe2_ls_SOURCES)
	@rm -f $@
	for src in $^; do \
          module="`basename $$src | sed 's/\.f90//'`"; \
	  grep '^ *use ' $$src \
	    | grep -v '!NODEP!' \
	    | sed -e 's/^ *use */'$$module'.lo: /' \
	          -e 's/, *only:.*//' \
	          -e 's/, *&//' \
	          -e 's/, *.*=>.*//' \
	          -e 's/ *$$/.lo/'; \
	  grep '^ *use ' $$src \
	    | grep -v '!NODEP!' \
	    | sed -e 's/^ *use */'$$module'.lo: /' \
	          -e 's/, *only:.*//' \
	          -e 's/, *&//' \
	          -e 's/, *.*=>.*//' \
	          -e 's/ *$$/.mod/'; \
	done > $@

DISTCLEANFILES = Makefile.depend

if OCAML_AVAILABLE

@am__include@ @am__quote@Makefile.depend_ocaml@am__quote@

# echo lexer.mli: lexer.ml >>$@
Makefile.depend_ocaml: $(CIRCE2_SRC) $(CIRCE2TOOL_SRC)
	@rm -f $@
	$(OCAMLDEP) -I $(srcdir) $^ | sed 's,[^ 	]*/,,g' >$@
	echo parser.mli: parser.ml >>$@
	echo lexer.cmi: parser.cmi >>$@
	echo parser.cmi: syntax.cmi >>$@
	echo commands.cmi: parser.cmi lexer.cmi >>$@
	echo commands.cmo: parser.cmi lexer.cmi >>$@
	echo commands.cmx: parser.cmx lexer.cmx >>$@
	echo lexer.cmo: lexer.cmi >>$@
	echo lexer.cmx: lexer.cmi parser.cmx >>$@
	echo parser.cmo: parser.cmi syntax.cmi >>$@
	echo parser.cmx: parser.cmi syntax.cmi syntax.cmx >>$@

DISTCLEANFILES += Makefile.depend_ocaml

DISTCLEANFILES += $(CIRCE2_DERIVED)

endif OCAML_AVAILABLE

########################################################################
## Non-standard cleanup tasks
## Remove sources that can be recreated using NOWEB
.PRECIOUS = $(MYPRECIOUS)

if NOWEB_AVAILABLE
maintainer-clean-noweb:
	-rm -f *.f90
endif
.PHONY: maintainer-clean-noweb

## Remove those sources also if builddir and srcdir are different
if NOWEB_AVAILABLE
clean-noweb:
	test "$(srcdir)" != "." && rm -f *.f90 || true
endif
.PHONY: clean-noweb

## Remove F90 module files
clean-local: clean-noweb
	-rm -f *.cm[aiox] *.cmxa *.[ao] *.l[oa] *.$(FC_MODULE_EXT) \
	*.g90 $(CIRCE2_NATIVE) $(CIRCE2_BYTECODE) $(CIRCE2_DERIVED)

## Remove backup files
maintainer-clean-backup:
	-rm -f *~
.PHONY: maintainer-clean-backup

## Register additional clean targets
maintainer-clean-local: maintainer-clean-noweb maintainer-clean-backup

###           module="`basename $$src | sed 's/\.f[90][0358]//'`"; 
########################################################################
# MPI
########################################################################
###
### # The -mismatch_all is for mpi_send() etc.
### MPIFC = mpif90
### MPIFCFLAGS = # -mismatch_all

