# Makefile.am -- Makefile for O'Mega within and without WHIZARD
# $Id: Makefile.am 2747 2010-08-09 16:27:25Z jr_reuter $
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

########################################################################
### TODO: fix weaving of lexers and parsers
########################################################################

include $(top_srcdir)/src/Makefile.sources

VPATH = $(srcdir):$(top_builddir)/src:$(srcdir):$(top_srcdir)/src

## This relies on conventions of Karl Berry's kpathsearch library.
## Is it safe to assume that all TeX installation use it?
MPINPUTS = .:$(srcdir):

PICTURES_EPS = \
    el_te_ph.eps \
    modules.eps \
    bhabha.eps bhabha0.eps \
    epemudbardubar.eps epemudbardubar0.eps \
    epemudbarmunumubar.eps epemudbarmunumubar0.eps

PICTURES_PDF = $(PICTURES_EPS:.eps=.pdf)

LATEX_STYLES = \
    flex.cls thophys.sty thohacks.sty \
    noweb.sty ocamlweb.sty \
    feynmp.sty feynmp.mp emp.sty

PDFS = omega.pdf preview.pdf

### Files needed to be installed with the O'Mega distribution
modelsdir = $(pkgdatadir)/doc
if SUPP_PDF_AVAILABLE
dist_doc_DATA = $(PDFS)
else
dist_doc_DATA = 
endif

EXTRA_DIST = $(PICTURES_EPS) $(LATEX_STYLES)

if NOWEB_AVAILABLE
pdf-local: $(PDFS)
else
pdf-local:
endif

SUFFIXES = .mly .mll .ml .implementation .mli .interface .nw .tex .dvi .eps .pdf .ps

if SUPP_PDF_AVAILABLE
if PDFLATEX_AVAILABLE
.tex.pdf:
	-$(PDFLATEX) $<
if MPOST_AVAILABLE
	MPINPUTS=$(MPINPUTS) $(MPOST) $*pics
	TEX=$(LATEX) $(MPOST) $*
endif MPOST_AVAILABLE
	echo "skipping -bibtex $*"
	$(PDFLATEX) $<
	while $(GREP) -s 'Rerun to get cross-references right.' $*.log; do \
	  $(PDFLATEX) $<; \
	done
else  !PDFLATEX_AVAILABLE
if LATEX_AVAILABLE
if DVIPS_AVAILABLE
if PS2PDF_AVAILABLE
	-$(LATEX) $<
if MPOST_AVAILABLE
	MPINPUTS=$(MPINPUTS) $(MPOST) $*pics
	TEX=$(LATEX) MPINPUTS=$(MPINPUTS) $(MPOST) $*
endif MPOST_AVAILABLE
	echo "skipping -bibtex $*"
	$(LATEX) $<
	while $(GREP) -s 'Rerun to get cross-references right.' $*.log; do \
	  $(LATEX) $<; \
	done
	$(DVIPS) $*
	$(PS2PDF) $*
endif PS2PDF_AVAILABLE
endif DVIPS_AVAILABLE
endif LATEX_AVAILABLE
endif !PDFLATEX_AVAILABLE
endif SUPP_PDF_AVAILABLE

if SUPP_PDF_AVAILABLE
if PDFLATEX_AVAILABLE
preview.pdf: el_te_ph.pdf modules.pdf
endif
endif

OMEGA_CORE_INTERFACES = $(OMEGA_CORE_MLI:.mli=.interface)
OMEGA_CORE_IMPLEMENTATIONS = $(OMEGA_CORE_ML:.ml=.implementation)
OMEGA_MODELLIB_INTERFACES = $(OMEGA_MODELLIB_MLI:.mli=.interface)
OMEGA_MODELLIB_IMPLEMENTATIONS = $(OMEGA_MODELLIB_ML:.ml=.implementation)
OMEGA_TARGETLIB_INTERFACES = $(OMEGA_TARGETLIB_MLI:.mli=.interface)
OMEGA_TARGETLIB_IMPLEMENTATIONS = $(OMEGA_TARGETLIB_ML:.ml=.implementation)
OMEGA_APPLICATIONS_IMPLEMENTATIONS = $(OMEGA_APPLICATIONS_ML:.ml=.implementation)

OMEGA_INTERFACES = \
    $(OMEGA_CORE_INTERFACES) \
    $(OMEGA_MODELLIB_INTERFACES) \
    $(OMEGA_TARGETLIB_INTERFACES)

OMEGA_IMPLEMENTATIONS = \
    $(OMEGA_CORE_IMPLEMENTATIONS) \
    $(OMEGA_MODELLIB_IMPLEMENTATIONS) \
    $(OMEGA_TARGETLIB_IMPLEMENTATIONS) \
    $(OMEGA_APPLICATIONS_IMPLEMENTATIONS)

if !NOWEB_AVAILABLE

omega.pdf:

else NOWEB_AVAILABLE

omega.pdf: \
    $(OMEGA_INTERFACES) $(OMEGA_IMPLEMENTATIONS) omegalib.tex index.tex \
    $(PICTURES_PDF) $(PICTURES_PS)

.nw.tex:
	$(NOWEAVE) -delay $< | $(CPIF) $@

if OCAMLWEB_AVAILABLE

.mll.implementation:
	$(OCAMLWEB) --no-preamble --noweb --no-index $< >$@

.mly.implementation:
	$(OCAMLWEB) --no-preamble --noweb --no-index $< >$@

.ml.implementation:
	$(OCAMLWEB) --no-preamble --noweb --no-index $< >$@

.mli.interface:
	$(OCAMLWEB) --no-preamble --noweb --no-index $< >$@

index.tex: $(OMEGA_CAML)
	$(OCAMLWEB) --no-preamble --noweb $^ | \
	sed -n '/\\ocwbeginindex{}/,/\\ocwendindex{}/p' >$@

endif OCAMLWEB_AVAILABLE

endif NOWEB_AVAILABLE

if EPSTOPDF_AVAILABLE
.eps.pdf:
	$(EPSTOPDF) --outfile=$@ $<
endif

########################################################################

## Cleanup tasks
mostlyclean-latex:
	-rm -f *.log *.aux *.toc *.mpx *.idx *.out omega*.mp preview*.mp \
	omegapics.t[0-9]* omegapics.[0-9]* $(PICTURES_PDF) $(PICTURES_PS) \
	omegalib.tex
clean-latex:
maintainer-clean-latex:
	-rm $(PDFS)
if NOWEB_AVAILABLE
mostlyclean-omega:	
	-test "$(srcdir)" != "." && -rm -f $(PDFS)
maintainer-clean-omega:
else
mostlyclean-omega:
maintainer-clean-omega:
endif 
.PHONY: mostlyclean-latex clean-latex maintainer-clean-latex
.PHONY: mostlyclean-omega maintainer-clean-omega

if OCAMLWEB_AVAILABLE
mostlyclean-caml:
	-rm -f $(OMEGA_INTERFACES) $(OMEGA_IMPLEMENTATIONS) index.tex
else
mostlyclean-caml:
endif
clean-caml:
if OCAMLWEB_AVAILABLE
maintainer-clean-caml:
	-rm -f $(OMEGA_INTERFACES) $(OMEGA_IMPLEMENTATIONS) index.tex
else 
maintainer-clean-caml:
endif
.PHONY: mostlyclean-caml clean-caml maintainer-clean-caml

## Remove backup files
maintainer-clean-backup:
	-rm -f *~
.PHONY: maintainer-clean-backup

## Register additional clean targets
mostlyclean-local: mostlyclean-latex mostlyclean-caml mostlyclean-omega
clean-local: clean-latex clean-caml
maintainer-clean-local: maintainer-clean-latex maintainer-clean-caml \
	maintainer-clean-omega maintainer-clean-backup

distclean-local:
	-test "$(srcdir)" != "." && rm -f $(LATEX_STYLES)

########################################################################
## The End.
########################################################################
