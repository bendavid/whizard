# Makefile.am --
# $Id: Makefile.am 317 2010-04-18 00:31:03Z ohl $
##
## Process this file with automake to produce Makefile.in
##
########################################################################
#
# Copyright (C) 1999-2014 by 
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

include $(top_srcdir)/src/Makefile.sources

WEBS = \
  prelude.nw circe2.nw postlude.nw

if DISTRIBUTION
PDFS = circe2.pdf 
else
PDFS = 
endif

LATEX_STYLES = \
    emp.sty noweb.sty thohacks.sty thopp.sty ocamlweb.sty

TEX_FLAGS = "$$TEXINPUTS:$(top_srcdir)/share/doc"
MP_FLAGS  = "$$MPINPUTS:$(top_srcdir)/share/doc"

CIRCE2PICS = circe2pics1.png circe2pics2.png circe2pics3.png \
	circe2pics4.png circe2pics5.png circe2pics6.png \
	circe2pics7.png circe2pics8.png circe2pics9.png

EXTRA_DIST = \
  tex-comments.sh \
  $(LATEX_STYLES) \
  $(CIRCE2PICS)

if DISTRIBUTION
dist_doc_DATA = $(PDFS)
endif

if NOWEB_AVAILABLE
pdf-local: circe2.pdf 
endif

VPATH = $(srcdir):$(top_builddir)/src:$(top_srcdir)/src

if NOWEB_AVAILABLE

circe2.tex: $(WEBS)
	$(NOWEAVE) -filter ./tex-comments -delay -index \
	    `for i in  $^; do case $$i in *.nw) echo $$i;; esac done` \
	  | $(CPIF) $@

circe2.tex: tex-comments

endif NOWEB_AVAILABLE

tex-comments: tex-comments.sh
	cp $< $@
	chmod +x $@

# preview.pdf: vegas.data vamp.data

# vegas.data: vegas.d
# 	cp $< $@
# 
# vamp.data: vamp.d
# 	cp $< $@

SUFFIXES = .tex .pdf .ml .implementation .mli .interface .nw .dvi .eps .ps

if !OCAMLWEB_AVAILABLE
circe2.pdf:
else
circe2.pdf: $(CIRCE2TOOLLIB_INTERFACE) $(CIRCE2TOOLLIB_IMPLEMENTATION) \
		$(CIRCE2TOOL_IMPLEMENTATION) index.tex
endif

if DISTRIBUTION
if PDFLATEX_AVAILABLE
if MPOST_AVAILABLE
if SUPP_PDF_AVAILABLE
if MAKEINDEX_AVAILABLE
.tex.pdf:
	-TEXINPUTS=$(TEX_FLAGS) $(PDFLATEX) $<
	-$(MAKEINDEX) -o $*.ind $*.idx
	-test -r $*.mp && TEX=$(LATEX) TEXINPUTS=$(TEX_FLAGS) $(MPOST) $*
	-test -r $*pics.mp && MPINPUTS=$(MP_FLAGS) $(MPOST) $*pics
	TEXINPUTS=$(TEX_FLAGS) $(PDFLATEX) $<
	if grep -s 'Rerun to get cross-references right.' $*.log; then \
	   TEXINPUTS=$(TEX_FLAGS) $(PDFLATEX) $<; \
	fi
else
.tex.pdf:
	-TEXINPUTS=$(TEX_FLAGS) $(PDFLATEX) $<
	-test -r $*.mp && TEX=$(LATEX) TEXINPUTS=$(TEX_FLAGS) $(MPOST) $*
	-test -r $*pics.mp && MPINPUTS=$(MP_FLAGS) $(MPOST) $*pics
	TEXINPUTS=$(TEX_FLAGS) $(PDFLATEX) $<
	if grep -s 'Rerun to get cross-references right.' $*.log; then \
	   TEXINPUTS=$(TEX_FLAGS) $(PDFLATEX) $<; \
	fi
endif
endif SUPP_PDF_AVAILABLE
endif MPOST_AVAILABLE
endif PDFLATEX_AVAILABLE
endif DISTRIBUTION

if OCAMLWEB_AVAILABLE
.ml.implementation:
	$(OCAMLWEB) --no-preamble --noweb --no-index $< >$@

.mli.interface:
	$(OCAMLWEB) --no-preamble --noweb --no-index $< >$@

index.tex: $(CIRCE2TOOLLIB_MLI) $(CIRCE2TOOLLIB_ML) $(CIRCE2TOOL_ML)
	$(OCAMLWEB) --no-preamble --noweb $^ | \
	   sed -n '/\\ocwbeginindex{}/,/\\ocwendindex{}/p' >$@
endif OCAMLWEB_AVAILABLE

## Cleanup tasks
mostlyclean-latex:
	-rm -f *.mpx *.[1-9]* *.t[1-9]* circe*.mp preview*.mp \
	   *.out *.log *.aux *.idx *.ilg *.ind tex-comments *.toc \
	   circe2.tex
clean-latex:
maintainer-clean-latex:
	-rm $(PDFS)
if NOWEB_AVAILABLE
if OCAMLWEB_AVAILABLE
mostlyclean-circe2:
	-test "$(srcdir)" != "." && rm -f $(PDFS)
maintainer-clean-circe2:
else
mostlyclean-circe2:
maintainer-clean-circe2:
endif
endif
.PHONY: mostlyclean-latex clean-latex maintainer-clean-latex
.PHONY: mostlyclean-circe2 maintainer-clean-circe2

if OCAMLWEB_AVAILABLE
mostlyclean-caml:
	-rm -f *.interface *.implementation index.tex
else
mostlyclean-caml:
endif
clean-caml:
if OCAMLWEB_AVAILABLE
maintainer-clean-caml:
	-rm -f *.interface *.implementation index.tex
else
maintainer-clean-caml:
endif
.PHONY: mostlyclean-caml clean-caml maintainer-clean-caml

## Remove backup files
maintainer-clean-backup:
	-rm -f *~
.PHONY: maintainer-clean-backup

## Register additional clean targets
mostlyclean-local: mostlyclean-latex mostlyclean-circe2 \
		mostlyclean-caml
clean-local: clean-latex clean-caml
maintainer-clean-local: maintainer-clean-latex maintainer-clean-circe2 \
		maintainer-clean-caml maintainer-clean-backup

if !DISTRIBUTION
install-data-hook: 
	-$(INSTALL) -m 644 circe2.pdf $(DESTDIR)$(datarootdir)/doc/circe2

uninstall-hook: 
	-rm -f $(DESTDIR)/$(datarootdir)/doc/circe2/circe2.pdf 
endif

########################################################################
## The End.
########################################################################

