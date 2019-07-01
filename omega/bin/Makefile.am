# Makefile.am -- Makefile for O'Mega within and without WHIZARD
# $Id: Makefile.am 6465 2015-01-10 15:22:31Z jr_reuter $
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

VPATH = $(top_builddir)/src:$(top_srcdir)/src

bin_SCRIPTS = $(OMEGA_APPLICATIONS_NATIVE)

bytecode: $(OMEGA_APPLICATIONS_BYTECODE)

OMEGA_CMXA = omega_core.cmxa omega_targets.cmxa omega_models.cmxa
OMEGA_CMA = $(OMEGA_CMXA:.cmxa=.cma)

########################################################################

include $(top_srcdir)/src/Makefile.ocaml
include $(top_srcdir)/src/Makefile.sources

OCAMLFLAGS += -I $(top_builddir)/src

$(OMEGA_APPLICATIONS_NATIVE): $(OMEGA_CMXA)
$(OMEGA_APPLICATIONS_BYTECODE): $(OMEGA_CMA)

omegatop: $(OMEGA_CMA)
	$(OCAMLMKTOP) $(OCAMLFLAGS) -o $@ unix.cma $(OMEGA_CMA)

########################################################################

SUFFIXES += $(OCAML_NATIVE_EXT) .$(OMEGA_CACHE_SUFFIX) .$(OMEGA_CACHE_SUFFIX_COLORED)

$(OCAML_NATIVE_EXT).$(OMEGA_CACHE_SUFFIX):
	@echo $@:
	./$< -initialize .

.$(OMEGA_CACHE_SUFFIX).$(OMEGA_CACHE_SUFFIX_COLORED):
	@echo $@:

if OMEGA_INSTALL_ALL_CACHES
OMEGA_INSTALLED_CACHES = $(OMEGA_CACHES)
else
OMEGA_INSTALLED_CACHES = $(OMEGA_MINIMAL_CACHES)
endif

install-data-local: $(OMEGA_INSTALLED_CACHES)
	$(INSTALL) -d -m 755 $(OMEGA_USER_CACHE_DIR)
	$(INSTALL) -d -m 755 $(DESTDIR)$(OMEGA_SYSTEM_CACHE_DIR)
	if test -w $(DESTDIR)$(OMEGA_SYSTEM_CACHE_DIR) ; then \
	  $(INSTALL) -m 644 -c $^ $(DESTDIR)$(OMEGA_SYSTEM_CACHE_DIR) ; \
	elif test -w $(OMEGA_USER_CACHE_DIR) ; then \
	  $(INSTALL) -m 644 -c $^ $(OMEGA_USER_CACHE_DIR) ; \
	fi

install-all-caches: $(OMEGA_CACHES)
	$(INSTALL) -d -m 755 $(OMEGA_USER_CACHE_DIR)
	$(INSTALL) -d -m 755 $(OMEGA_SYSTEM_CACHE_DIR)
	if test -w $(OMEGA_SYSTEM_CACHE_DIR) ; then \
	  $(INSTALL) -m 644 -c $^ $(OMEGA_SYSTEM_CACHE_DIR) ; \
	elif test -w $(OMEGA_USER_CACHE_DIR) ; then \
	  $(INSTALL) -m 644 -c $^ $(OMEGA_USER_CACHE_DIR) ; \
	fi

uninstall-local:
	for cache in $(OMEGA_CACHES); do \
	  rm -f $(DESTDIR)$(OMEGA_SYSTEM_CACHE_DIR)/$$cache $(OMEGA_USER_CACHE_DIR)/$$cache; \
	done

clean-local:
	rm -f *.cm[iox] *.o \
	  $(OMEGA_APPLICATIONS_BYTECODE) $(OMEGA_APPLICATIONS_NATIVE) \
	  $(OMEGA_CACHES)

########################################################################
## The End.
########################################################################
