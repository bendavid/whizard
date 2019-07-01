# Makefile.am -- Makefile for O'Mega within and without WHIZARD
# $Id: Makefile.am 5979 2014-07-04 07:41:46Z jr_reuter $
##
## Process this file with automake to produce Makefile.in
##
########################################################################
#
# Copyright (C) 1999-2014 by 
#     Wolfgang Kilian <kilian@physik.uni-siegen.de>
#     Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
#     Juergen Reuter <juergen.reuter@desy.de>
#     with contributions from
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

########################################################################
### TODO: fix weaving of lexers and parsers
########################################################################

include $(top_srcdir)/src/Makefile.sources

VPATH = $(srcdir):$(top_builddir)/src:$(srcdir):$(top_srcdir)/src

PICTURES_EPS = \
    modules.eps \
    omega-paper-1-pics-1.eps \
    omega-paper-1-pics-2.eps \
    omega-paper-1-pics-3.eps \
    omega-paper-1-pics-4.eps \
    omega-paper-1-pics-5.eps \
    omega-paper-1-pics-6.eps \
    omega-paper-1-pics-7.eps \
    omega-paper-1-pics-8.eps \
    omega-paper-1-pics-9.eps \
    omega-paper-1-pics-10.eps \
    bhabha.eps bhabha0.eps \
    epemudbardubar.eps epemudbardubar0.eps \
    epemudbarmunumubar.eps epemudbarmunumubar0.eps \
    sign_ex.eps fusion_rules.eps mom_choice.eps \
    mom_flow.eps

PICTURES_PDF = $(PICTURES_EPS:.eps=.pdf)

LATEX_STYLES = \
    flex.cls thophys.sty thohacks.sty \
    noweb.sty ocamlweb.sty \
    feynmp.sty feynmp.mp emp.sty

TEX_FLAGS = "$$TEXINPUTS:$(top_srcdir)/share/doc"
MP_FLAGS  = "$$MPINPUTS:$(top_srcdir)/share/doc"

if DISTRIBUTION
PDFS = omega.pdf omega-paper-1.pdf omega-paper-2.pdf 
else
PDFS = 
endif

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

if DISTRIBUTION
if SUPP_PDF_AVAILABLE
if PDFLATEX_AVAILABLE
.tex.pdf:
	-TEXINPUTS=$(TEX_FLAGS) $(PDFLATEX) $<
if MPOST_AVAILABLE
	-test -r $*pics.mp && MPINPUTS=$(MP_FLAGS) $(MPOST) $*pics
	-test -r $*.mp && TEX=$(LATEX) TEXINPUTS=$(TEX_FLAGS) MPINPUTS=$(MP_FLAGS) $(MPOST) $*
endif MPOST_AVAILABLE
	echo "skipping -bibtex $*"
	TEXINPUTS=$(TEX_FLAGS) $(PDFLATEX) $<
	while $(GREP) -s 'Rerun to get cross-references right.' $*.log; do \
	  TEXINPUTS=$(TEX_FLAGS) $(PDFLATEX) $<; \
	done
else  !PDFLATEX_AVAILABLE
if LATEX_AVAILABLE
if DVIPS_AVAILABLE
if PS2PDF_AVAILABLE
	-TEXINPUTS=$(TEX_FLAGS) $(LATEX) $<
if MPOST_AVAILABLE
	MPINPUTS=$(MP_FLAGS) $(MPOST) $*pics
	TEX=$(LATEX) TEXINPUTS=$(TEX_FLAGS) MPINPUTS=$(MP_FLAGS) $(MPOST) $*
endif MPOST_AVAILABLE
	echo "skipping -bibtex $*"
	TEXINPUTS=$(TEX_FLAGS) $(LATEX) $<
	while $(GREP) -s 'Rerun to get cross-references right.' $*.log; do \
	  TEXINPUTS=$(TEX_FLAGS) $(LATEX) $<; \
	done
	$(DVIPS) -o $*.ps $*
	$(PS2PDF) $*
endif PS2PDF_AVAILABLE
endif DVIPS_AVAILABLE
endif LATEX_AVAILABLE
endif !PDFLATEX_AVAILABLE
endif SUPP_PDF_AVAILABLE
endif DISTRIBUTION

if DISTRIBUTION
if SUPP_PDF_AVAILABLE
if PDFLATEX_AVAILABLE

omega-paper-1.pdf: modules.pdf \
    omega-paper-1-pics-1.pdf \
    omega-paper-1-pics-2.pdf \
    omega-paper-1-pics-3.pdf \
    omega-paper-1-pics-4.pdf \
    omega-paper-1-pics-5.pdf \
    omega-paper-1-pics-6.pdf \
    omega-paper-1-pics-7.pdf \
    omega-paper-1-pics-8.pdf \
    omega-paper-1-pics-9.pdf \
    omega-paper-1-pics-10.pdf

# Dependencies and avoid mpost race condition
omega-paper-2.pdf: \
    omega-paper-1.pdf sign_ex.pdf fusion_rules.pdf \
    mom_choice.pdf mom_flow.pdf

endif PDFLATEX_AVAILABLE
endif SUPP_PDF_AVAILABLE
endif DISTRIBUTION

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
    $(PICTURES_PDF) $(PICTURES_EPS)

.nw.tex:
	$(NOWEAVE) -delay $< | $(CPIF) $@

if DISTRIBUTION
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
endif DISTRIBUTION

endif NOWEB_AVAILABLE

if EPSPDF_AVAILABLE
if EPSPDF_043
.eps.pdf:
	$(EPSPDF) $< $@ 
else
if EPSTOPDF_AVAILABLE
.eps.pdf:
	$(EPSTOPDF) --outfile=$@ $<
endif
endif
else
if EPSTOPDF_AVAILABLE
.eps.pdf:
	$(EPSTOPDF) --outfile=$@ $<
endif
endif

########################################################################

## Cleanup tasks
mostlyclean-latex:
	-rm -f *.log *.aux *.toc *.mpx *.idx *.out omega*.mp \
	omega*pics.t[0-9]* omega*pics.[0-9]* $(PICTURES_PDF) \
	omegalib.tex
clean-latex:
maintainer-clean-latex:
	-rm $(PDFS)
if NOWEB_AVAILABLE
mostlyclean-omega:	
	-test "$(srcdir)" != "." && rm -f $(PDFS)
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

if !DISTRIBUTION
install-data-hook: 
	-$(INSTALL) -m 644 omega.pdf $(DESTDIR)$(datarootdir)/doc/omega
	-$(INSTALL) -m 644 omega-paper-1.pdf $(DESTDIR)$(datarootdir)/doc/omega
	-$(INSTALL) -m 644 omega-paper-2.pdf $(DESTDIR)$(datarootdir)/doc/omega

uninstall-hook: 
	-rm -f $(DESTDIR)/$(datarootdir)/doc/omega/omega.pdf 
	-rm -f $(DESTDIR)/$(datarootdir)/doc/omega/omega-paper-1.pdf 
	-rm -f $(DESTDIR)/$(datarootdir)/doc/omega/omega-paper-2.pdf 
endif

########################################################################
## The End.
########################################################################
