## Makefile.am -- Makefile for WHIZARD
# $Id: Makefile.am 1564 2010-01-21 18:19:23Z ohl $
##
## Process this file with automake to produce Makefile.in
##
########################################################################
#
# Copyright (C) 1999-2010 by 
#     Wolfgang Kilian <kilian@hep.physik.uni-siegen.de>
#     Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
#     Juergen Reuter <juergen.reuter@physik.uni-freiburg.de>
#     Christian Speckner <christian.speckner@physik.uni-freiburg.de>
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

## The files in this directory end up in an auxiliary libtool library.

if PYTHIA_AVAILABLE

if FC_IS_NAG
AM_FFLAGS = -dcfuns -w
endif

noinst_LTLIBRARIES = libpythia_wo.la

libpythia_wo_la_SOURCES = pythia.f

endif	

## Remove backup files
maintainer-clean-local:
	-rm -f *~