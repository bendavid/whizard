# Makefile.am --
# $Id: Makefile.am 317 2010-04-18 00:31:03Z ohl $
########################################################################
#
# Copyright (C) 1999-2011 by 
#     Wolfgang Kilian <kilian@physik.uni-siegen.de>
#     Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
#     Juergen Reuter <juergen.reuter@desy.de>
#     Christian Specker <christian.speckner@physik.uni-freiburg.de>
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


WEBS = \
  prelude.nw \
  circe1.nw minuit.nw postlude.nw

if DISTRIBUTION
PDFS = circe1.pdf 
else
PDFS = 
endif

PICTURES_EPS = \
    dist1.eps dist2.eps dist3.eps \
    dist4.eps dist5.eps dist6.eps \
    figures1.eps fit11.eps fit12.eps \
    fit21.eps fit22.eps fit13.eps \
    fit23.eps fit15.eps fit25.eps

PICTURES_PDF = $(PICTURES_EPS:.eps=.pdf)

LATEX_STYLES = \
    noweb.sty thohacks.sty thopp.sty

EXTRA_DIST = \
  tex-comments.sh \
  $(PICTURES_EPS) \
  $(LATEX_STYLES)

dist_doc_DATA = $(PDFS)

if NOWEB_AVAILABLE
pdf-local: circe1.pdf 
endif

VPATH = $(srcdir):$(top_builddir)/src:$(top_srcdir)/src

if NOWEB_AVAILABLE

circe1.tex: $(WEBS)
	$(NOWEAVE) -filter ./tex-comments -delay -index \
	    `for i in  $^; do case $$i in *.nw) echo $$i;; esac done` \
	  | $(CPIF) $@

circe1.tex: tex-comments

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

SUFFIXES = .tex .pdf

MPOST_LATEX = TEX=$(LATEX) $(MPOST)

if DISTRIBUTION
if PDFLATEX_AVAILABLE
if MPOST_AVAILABLE
if SUPP_PDF_AVAILABLE
.tex.pdf:
	-pdflatex $<
	makeindex -o $*.ind $*.idx
	-test -r $*.mp && $(MPOST_LATEX) $*
	-test -r $*pics.mp && $(MPOST) $*pics
	pdflatex $<
	if grep -s 'Rerun to get cross-references right.' $*.log; then \
	  pdflatex $<; \
	fi
endif SUPP_PDF_AVAILABLE
endif MPOST_AVAILABLE
endif PDFLATEX_AVAILABLE
endif DISTRIBUTION

if DISTRIBUTION
if SUPP_PDF_AVAILABLE
if PDFLATEX_AVAILABLE
circe1.pdf: $(PICTURES_PDF)
endif PDFLATEX_AVAILABLE
endif SUPP_PDF_AVAILABLE
endif DISTRIBUTION

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
	-rm -f *.data *.rcs *.mpx *.[1-9] *.t[1-9] circe*.mp preview*.mp \
		circe1.tex *.out *.log *.aux *.idx *.ilg *.ind *.toc \
		$(PICTURES_PDF) tex-comments
	-test "$(srcdir)" != "." && rm -f $(PDFS)
clean-latex:
maintainer-clean-latex: 
	-rm -f $(PDFS)
if NOWEB_AVAILABLE
mostlyclean-circe1:
	-test "$(srcdir)" != "." && rm -f $(PDFS)
maintainer-clean-circe1:
else
mostlyclean-circe1:
maintainer-clean-circe1:
endif
.PHONY: mostlyclean-latex clean-latex maintainer-clean-latex
.PHONY: mostlyclean-circe1 maintainer-clean-circe1

## Remove backup files
maintainer-clean-backup:
	-rm -f *~
.PHONY: maintainer-clean-backup

## Register additional clean targets
mostlyclean-local: mostlyclean-latex mostlyclean-circe1
clean-local: clean-latex 
maintainer-clean-local: maintainer-clean-backup \
	maintainer-clean-circe1 maintainer-clean-latex

if !DISTRIBUTION
install-data-hook: 
	-$(INSTALL) -m 644 circe1.pdf $(DESTDIR)$(datarootdir)/doc/circe1

uninstall-hook: 
	-rm -f $(DESTDIR)/$(datarootdir)/doc/circe1/circe1.pdf 
endif

distclean-local:
	-test "$(srcdir)" != "." && rm -f $(LATEX_STYLES)

########################################################################
## The End.
########################################################################

