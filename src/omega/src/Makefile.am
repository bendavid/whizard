# Makefile.am -- Makefile for O'Mega within and without WHIZARD
# $Id: Makefile.am 2034 2010-03-08 18:24:39Z cnspeckn $
##
## Process this file with automake to produce Makefile.in
##
########################################################################
#
# Copyright (C) 1999-2010 by 
#     Wolfgang Kilian <kilian@hep.physik.uni-siegen.de>
#     Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
#     Juergen Reuter <juergen.reuter@physik.uni-freiburg.de>
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

# Build the O'Mega Fortran library using libtool
# (use pkglib_ instead of lib_ to make the -rpath and *.lai business work ...)
pkglib_LTLIBRARIES = libomega_core.la
pkginclude_DATA = $(OMEGALIB_MOD)

libomega_core_la_SOURCES = $(OMEGALIB_F90)

EXTRA_DIST = \
    $(OMEGA_CAML) \
    omegalib.nw $(OMEGALIB_F90)

OMEGA_CMXA = omega_core.cmxa omega_targets.cmxa omega_models.cmxa
OMEGA_CMA = $(OMEGA_CMXA:.cmxa=.cma)

if OCAML_AVAILABLE 
all-local: $(OMEGA_CMXA) $(OMEGA_APPLICATIONS_CMX)
bytecode: $(OMEGA_CMA) $(OMEGA_APPLICATIONS_CMO)
else
all-local: 
bytecode: 
endif

# Compiled interfaces and libraries for out-of-tree compilation of models
if OCAML_AVAILABLE
camllibdir = $(pkglibdir)/caml
nodist_camllib_DATA = \
	omega.cmi fusion.cmi targets.cmi coupling.cmi modeltools.cmi color.cmi \
	options.cmi rCS.cmi model.cmi \
	omega_core.cmxa omega_core.a omega_targets.cmxa omega_targets.a
endif

########################################################################

include $(top_srcdir)/src/Makefile.ocaml
include $(top_srcdir)/src/Makefile.sources
include $(top_srcdir)/src/Makefile.depend

if OCAML_AVAILABLE 
omega_core.a: omega_core.cmxa
omega_core.cmxa: $(OMEGA_CORE_CMX)
	$(OCAMLOPT) $(OCAMLFLAGS) $(OCAMLOPTFLAGS) -a -o $@ $^

omega_core.cma: $(OMEGA_CORE_CMO)
	$(OCAMLC) $(OCAMLFLAGS) -a -o $@ $^

omega_targets.a: omega_targets.cmxa
omega_targets.cmxa: $(OMEGA_TARGETS_CMX)
	$(OCAMLOPT) $(OCAMLFLAGS) $(OCAMLOPTFLAGS) -a -o $@ $^

omega_targets.cma: $(OMEGA_TARGETS_CMO)
	$(OCAMLC) $(OCAMLFLAGS) -a -o $@ $^

omega_models.cmxa: $(OMEGA_MODELS_CMX)
	$(OCAMLOPT) $(OCAMLFLAGS) $(OCAMLOPTFLAGS) -a -o $@ $^

omega_models.cma: $(OMEGA_MODELS_CMO)
	$(OCAMLC) $(OCAMLFLAGS) -a -o $@ $^

cascade_lexer.mli: cascade_lexer.ml cascade_parser.cmi
	$(OCAMLC) -i $< | $(GREP) 'val token' >$@

comphep_lexer.mli: comphep_lexer.ml comphep_parser.cmi
	$(OCAMLC) -i $< | $(GREP) 'val token' >$@

vertex_lexer.mli: vertex_lexer.ml vertex_parser.cmi
	$(OCAMLC) -i $< | $(GREP) 'val token' >$@

model_file_lexer.mli: model_file_lexer.ml model_file_parser.cmi
	$(OCAMLC) -i $< | $(GREP) 'val token' >$@
endif

MYPRECIOUS = $(OMEGA_DERIVED_CAML)

SUFFIXES += .lo .$(FC_MODULE_EXT)

# Fortran90 module files are generated at the same time as object files
.lo.$(FC_MODULE_EXT):
	@:
#	touch $@

########################################################################

$(OMEGALIB_DERIVED_F90): omegalib.stamp

if NOWEB_AVAILABLE

omegalib.stamp: $(srcdir)/omegalib.nw
	@rm -f omegalib.tmp
	@touch omegalib.tmp
	for src in $(OMEGALIB_DERIVED_F90); do \
	  $(NOTANGLE) -R[[$$src]] $< | $(CPIF) $$src; \
	done
	@mv -f omegalib.tmp omegalib.stamp

else

omegalib.stamp:


endif

MYPRECIOUS += $(OMEGALIB_DERIVED_F90)

########################################################################
# Don't trigger remakes by deleting intermediate files.
.PRECIOUS = $(MYPRECIOUS)

clean-local:
	rm -f *.cm[aiox] *.cmxa *.[ao] *.l[oa] *.$(FC_MODULE_EXT) \
		$(OMEGA_DERIVED_CAML) \
		$(OMEGALIB_DERIVED_F90) omegalib.stamp

distclean-local:
	-test "$(srcdir)" != "." && rm -f config.mli

########################################################################
## The End.
########################################################################
