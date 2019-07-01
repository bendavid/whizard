## Makefile.am -- Makefile for WHIZARD API
##
## Process this file with automake to produce Makefile.in
##
########################################################################
#
# Copyright (C) 1999-2014 by
#     Wolfgang Kilian <kilian@physik.uni-siegen.de>
#     Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
#     Juergen Reuter <juergen.reuter@desy.de>
# with contributions from
#     Christian Speckner <cnspeckn@googlemail.com>
#     Steffen Schwertfeger (only this file)
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

noinst_PROGRAMS = c_example cpp_example

c_example_SOURCES = c_test.c
c_example_LDADD  = $(top_builddir)/src/libwhizard.la
c_example_LDADD += $(top_builddir)/src/prebuilt/libwhizard_prebuilt.la
c_example_LDADD += $(LDFLAGS_LHAPDF)
c_example_LDADD += $(LDFLAGS_STDHEP)
c_example_LDADD += $(LDFLAGS_HEPMC)
c_example_LDADD += $(LDFLAGS_LCIO)
c_example_LDADD += $(LDFLAGS_HOPPET)

cpp_example_SOURCES = cpp_whizard.cc cpp_test.cc cpp_whizard.h
cpp_example_LDADD  = $(top_builddir)/src/libwhizard.la
cpp_example_LDADD += $(top_builddir)/src/prebuilt/libwhizard_prebuilt.la
cpp_example_LDADD += $(LDFLAGS_LHAPDF)
cpp_example_LDADD += $(LDFLAGS_STDHEP)
cpp_example_LDADD += $(LDFLAGS_HEPMC)
cpp_example_LDADD += $(LDFLAGS_LCIO)
cpp_example_LDADD += $(LDFLAGS_HOPPET)

SUFFIXES: .c .cc .py

python: py_whizard_copy
	$(PYTHON_BIN) py_whiz_setup.py build_ext --inplace

PYTHON_FROM_SRC = py_whizard.c test.py

py_whizard_copy: $(top_srcdir)/share/interfaces/py_whizard.c
	-if test "$(srcdir)" != "."; then \
	for file in $(PYTHON_FROM_SRC); do \
	   test -f "$$file" || cp $(srcdir)/$$file .; \
	done; \
	fi

## Remove files
clean-local:
	-rm -f eescat*
	-rm -f EEScat*
	-rm -f processes.*
	-rm -f *.log c_example
	-rm -f default_lib*
	-rm -f opr_*
	-rm -rf build
	-rm -f py_whizard.c

## Remove backup files
maintainer-clean-local:
	-rm -f *~
