# Makefile.am -- Makefile for O'Mega within and without WHIZARD
# $Id: Makefile.am 7218 2015-09-08 21:42:34Z jr_reuter $
##
## Process this file with automake to produce Makefile.in
##
########################################################################
#
# Copyright (C) 1999-2015 by 
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

## we do our own dependency checking for FORTRAN and OCaml:
AUTOMAKE_OPTIONS = no-dependencies

## Use the m4 directory for local Autoconf macros
ACLOCAL_AMFLAGS = -I m4

## Subdirectories to configure
if OCAML_AVAILABLE 
OPT_OMEGA = bin models share tests extensions
else
OPT_OMEGA = 
endif
SUBDIRS = src lib $(OPT_OMEGA) tools scripts
DIST_SUBDIRS = src lib models bin share tools extensions tests scripts

## Use the same compiler and compiler flags for distcheck
DISTCHECK_CONFIGURE_FLAGS = FC=$(FC) FCFLAGS="$(FCFLAGS)" LIBS="$(LIBS)" \
	--enable-distribution

if FC_IS_NAG
EXTRA_DISTCHECK_CONFIGURE_FLAGS = \
    "--disable-noweb-force" \
    "--disable-noweb-force --disable-ocaml" \
    "--disable-noweb-force --disable-shared" \
    "--disable-noweb-force --disable-static"
else
if FC_IS_GFORTRAN
EXTRA_DISTCHECK_CONFIGURE_FLAGS = \
    "--disable-noweb-force" \
    "--disable-noweb-force --disable-ocaml" \
    "--disable-noweb-force --disable-shared" \
    "--disable-noweb-force --disable-static" \
    "--disable-noweb-force --with-precision=extended" \
    "--disable-noweb-force --with-precision=quadruple"
else
EXTRA_DISTCHECK_CONFIGURE_FLAGS = \
    "--disable-noweb-force" \
    "--disable-noweb-force --disable-ocaml" \
    "--disable-noweb-force --disable-shared" \
    "--disable-noweb-force --disable-static" \
    "--disable-noweb-force --with-precision=quadruple"
endif
endif

extra-distcheck:
	@echo "================================================="  >$@.log
	@echo "make distcheck with additional configure options:" >>$@.log
	for flag in $(EXTRA_DISTCHECK_CONFIGURE_FLAGS); do \
	  if $(MAKE) $(AM_MAKEFLAGS) \
	        DISTCHECK_CONFIGURE_FLAGS='$(DISTCHECK_CONFIGURE_FLAGS) '"$$flag" \
	        distcheck; then \
	    echo "PASS $$flag" >>$@.log; \
	  else \
	    echo "FAIL $$flag" >>$@.log; \
	  fi; \
	done
	@echo "=================================================" >>$@.log
	@cat $@.log
	@rm -f $@.log

########################################################################

clean-local:
#	rm -fr var

if OCAML_AVAILABLE
install-all-caches:
	cd bin && $(MAKE) $(AM_MAKEFLAGS) install-all-caches

install-data-local:
	$(INSTALL) -d -m 755 $(OMEGA_USER_CACHE_DIR)
	$(INSTALL) -d -m 755 $(DESTDIR)$(OMEGA_SYSTEM_CACHE_DIR)
endif

uninstall-local:
	rm -f $(OMEGA_USER_CACHE_DIR)/*$(OMEGA_CACHE_SUFFIX)
	rm -f $(OMEGA_SYSTEM_CACHE_DIR)/*$(OMEGA_CACHE_SUFFIX)

DISTCLEANFILES = \
  iso_c_binding_gfortran.$(FC_MODULE_EXT) \
  iso_fortran_env_2008.$(FC_MODULE_EXT) \
  query_kinds.$(FC_MODULE_EXT) \
  report_kinds.$(FC_MODULE_EXT)

########################################################################
## The End.
########################################################################

