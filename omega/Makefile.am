# Makefile.am -- Makefile for O'Mega within and without WHIZARD
##
## Process this file with automake to produce Makefile.in
##
########################################################################
#
# Copyright (C) 1999-2017 by 
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

########################################################################
## The End.
########################################################################

