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

bin_SCRIPTS = 

if OCAML_AVAILABLE
if CAMLP4_AVAILABLE
bin_SCRIPTS += $(CIRCE2_NATIVE)
endif CAMLP4_AVAILABLE
endif OCAML_AVAILABLE

$(CIRCE2_NATIVE): $(CIRCE2_CMXA)
$(CIRCE2_BYTECODE): $(CIRCE2_CMA)

# Compiled interfaces and libraries for out-of-tree compilation of models
if OCAML_AVAILABLE
if CAMLP4_AVAILABLE
camllibdir = $(pkglibdir)/caml
nodist_camllib_DATA = \
	division.cmi events.cmi grid.cmi histogram.cmi \
	commands.cmi float.cmi diffmap.cmi \
	circe2_tool.cmxa circe2_tool.a 
endif CAMLP4_AVAILABLE
endif OCAML_AVAILABLE

if OCAML_AVAILABLE
if CAMLP4_AVAILABLE
all-local: $(CIRCE2_CMXA) $(CIRCE2TOOL_CMX)
bytecode: $(CIRCE2_CMA) $(CIRCE2TOOL_CMO)
else
all-local:
bytecode:
endif 
endif

include $(top_srcdir)/src/Makefile.ocaml
include $(top_srcdir)/src/Makefile.sources

SOURCE_FILES = \
  kinds.f90 circe2.f90 taorng.f90

EXTRA_SOURCE_FILES = 

MODULE_FILES = \
  kinds.$(FC_MODULE_EXT) \
  circe2.$(FC_MODULE_EXT) 

NOWEB_FILES = \
    prelude.nw postlude.nw circe2.nw

EXTRA_DIST = $(NOWEB_FILES) $(SOURCE_FILES) $(CIRCE2_CAML) \
	$(EXTRA_SOURCE_FILES) circe2d.f90

# Fortran90 module files are generated at the same time as object files
.lo.$(FC_MODULE_EXT):
	@:
#	touch $@

lib_LTLIBRARIES = libcirce2.la libcirce2d.la libcirce2_c.la 
if CXX_AVAILABLE
lib_LTLIBRARIES += libcirce2_cpp.la
endif CXX_AVAILABLE
libcirce2_la_SOURCES = $(SOURCE_FILES)
libcirce2d_la_SOURCES = circe2d.f90
libcirce2_c_la_SOURCES = circe2_c.c
libcirce2_cpp_la_SOURCES = circe2_cpp.cc

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

circe2d.f90: $(WEBS)
	cat $(WEBS) | $(NOTANGLE) -R'[[$@]]' | $(CPIF) $@

taorng.f90: postlude.nw
	cat $(WEBS) | $(NOTANGLE) -R'[[$@]]' | $(CPIF) $@

circe2_c.c: $(WEBS)
	cat $(WEBS) | $(NOTANGLE) -R'[[$@]]' | $(CPIF) $@

circe2_cpp.cc: $(WEBS)
	cat $(WEBS) | $(NOTANGLE) -R'[[$@]]' | $(CPIF) $@
endif NOWEB_AVAILABLE

########################################################################
# O'Caml
########################################################################

CIRCE2_CMXA = circe2_tool.cmxa
CIRCE2_CMA = $(CIRCE2_CMXA:.cmxa=.cma)

if OCAML_AVAILABLE
if CAMLP4_AVAILABLE
circe2_tool.a: circe2_tool.cmxa
circe2_tool.cmxa: $(CIRCE2TOOLLIB_CMX) $(CIRCE2TOOL_CMX)
	$(OCAMLOPT) $(OCAMLFLAGS) $(OCAMLOPTFLAGS) -a -o $@ $^
circe2_tool.cma: $(CIRCE2TOOLLIB_CMO)
	$(OCAMLC) $(OCAMLFLAGS) -a -o $@ $^
circe2.top: $(CIRCE2TOOLLIB_CMO)
	$(OCAMLMKTOP) $(OCAMLFLAGS) -o $@ \
		unix.cma bigarray.cma $(CIRCE2TOOLLIB_CMO)
endif CAMLP4_AVAILABLE
endif OCAML_AVAILABLE

########################################################################
# The following line just says
#    include Makefile.depend
# but in a portable fashion (depending on automake's AM_MAKE_INCLUDE
########################################################################
@am__include@ @am__quote@Makefile.depend@am__quote@

Makefile.depend: $(SOURCE_FILES)
	@rm -f $@
	for src in $^; do \
          module="`basename $$src | sed 's/\.f90//'`"; \
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

if OCAML_AVAILABLE

@am__include@ @am__quote@Makefile.depend_ocaml@am__quote@

Makefile.depend_ocaml: $(CIRCE2_CAML)
	@rm -f $@
	$(OCAMLDEP) $(STREAM_PARSER) -I $(srcdir) $^ \
	  $(CIRCE2_DERIVED_CAML) | sed 's,[^ 	]*/,,g' > $@

DISTCLEANFILES += Makefile.depend_ocaml

endif OCAML_AVAILABLE

########################################################################
## Non-standard cleanup tasks
## Remove sources that can be recreated using NOWEB
if NOWEB_AVAILABLE
maintainer-clean-noweb:
	-rm -f *.f90 *.c *.cc
endif
.PHONY: maintainer-clean-noweb

## Remove those sources also if builddir and srcdir are different
if NOWEB_AVAILABLE
clean-noweb:
	test "$(srcdir)" != "." && rm -f *.f90 *.c *.cc || true
endif
.PHONY: clean-noweb

## Remove F90 module files
clean-local: clean-noweb
	-rm -f *.cm[aiox] *.cmxa *.[ao] *.l[oa] *.$(FC_MODULE_EXT) \
	*.g90 $(CIRCE2_NATIVE) $(CIRCE2_BYTECODE)

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

