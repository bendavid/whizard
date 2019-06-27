# Makefile.am --
# $Id: Makefile.am 319 2010-04-18 15:00:54Z ohl $
########################################################################

SUBDIRS = src share tests

## Use the same compiler and compiler flags for distcheck
DISTCHECK_CONFIGURE_FLAGS = FC=$(FC) FCFLAGS="$(FCFLAGS)" LIBS="$(LIBS)"

## Use the m4 directory for local Autoconf macros
ACLOCAL_AMFLAGS = -I m4
