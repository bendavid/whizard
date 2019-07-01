## Makefile.am -- Makefile for WHIZARD
##
## Process this file with automake to produce Makefile.in
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

## PYTHIA is automatically available if matching is switched on

if FC_IS_NAG
AM_FFLAGS = -dcfuns -w
endif

if MATCHING_AVAILABLE
bin_PROGRAMS = interface

interface_LDADD = ../pythia/libpythia_wo.la
interface_LDFLAGS = -I../pythia

if STDHEP_AVAILABLE
interface_SOURCES = main_stdhep.f interface.f
interface_LDFLAGS += $(LDFLAGS_STDHEP)
else
interface_SOURCES = main_lhef.f interface.f 
endif	

else
bin_PROGRAMS = 
endif

