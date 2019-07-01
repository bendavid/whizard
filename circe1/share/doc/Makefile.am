# Makefile.am --
# $Id: Makefile.am 317 2010-04-18 00:31:03Z ohl $
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

CIRCE1_VER = 1
CIRCE1_REV = 0

WEBS = \
  prelude.nw \
  circe1.nw minuit.nw postlude.nw

if DISTRIBUTION
PDFS = circe1.pdf 
else
PDFS = 
endif

PICTURES_EPS = \
    figures1.eps fit11.eps fit12.eps \
    fit21.eps fit22.eps fit13.eps \
    fit23.eps fit15.eps fit25.eps \
    dist78.eps 

PICTURES_PDF = $(PICTURES_EPS:.eps=.pdf)

LATEX_STYLES = \
    noweb.sty thohacks.sty thopp.sty

TEX_FLAGS = "$$TEXINPUTS:$(top_srcdir)/share/doc"
MP_FLAGS  = "$$MPINPUTS:$(top_srcdir)/share/doc"

MP4_FILES = \
    circemacs.mp4 dist.mp4 fit.mp4 graphx.mp

EXTRA_DIST = \
  tex-comments.sh \
  $(PICTURES_EPS) \
  $(LATEX_STYLES) \
  $(MP4_FILES)

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

.mp4.mp: circemacs.mp4
	$(M4) -I$(top_srcdir)/share/doc $< >$@

########################################################################
# Old targets for the fit plots
########################################################################

fit.mp dist.mp: circemacs.mp4

fit.11: fit.mp
	TEX=$(LATEX) TEXINPUTS=$(TEX_FLAGS) $(MPOST) $<


# preview.pdf: vegas.data vamp.data

# vegas.data: vegas.d
# 	cp $< $@
# 
# vamp.data: vamp.d
# 	cp $< $@


########################################################################
# The distribution plots for the write-up
########################################################################

dist.1: dist.mp \
  de-sband.dat dg-sband.dat de-tesla.dat dg-tesla.dat \
  de-xband.dat dg-xband.dat de-sbandt.dat dg-sbandt.dat \
  de-teslat.dat dg-teslat.dat de-xbandt.dat dg-xbandt.dat \
  de-tesla3.dat dg-tesla3.dat de-tesla8.dat dg-tesla8.dat \
  de-sband-ee.dat dg-sband-ee.dat de-tesla-ee.dat dg-tesla-ee.dat \
  de-xband-ee.dat dg-xband-ee.dat de-sbandt-ee.dat dg-sbandt-ee.dat \
  de-teslat-ee.dat dg-teslat-ee.dat de-xbandt-ee.dat dg-xbandt-ee.dat
	TEX=$(LATEX) TEXINPUTS=$(TEX_FLAGS) \
	MPINPUTS=$(MP_FLAGS) $(MPOST) $<

########################################################################
# Generating CIRCE data
########################################################################

de-sband.dat: $(top_builddir)/tools/circe1_plot
	echo 1 0.001 1.0 50 -1.0 11 11 500.0 1 $(CIRCE1_VER) $(CIRCE1_REV) | $(CPLOT)

dg-sband.dat: $(top_builddir)/tools/circe1_plot
	echo 1 0.001 1.0 50 -1.0 22 11 500.0 1 $(CIRCE1_VER) $(CIRCE1_REV) | $(CPLOT)

de-tesla.dat: $(top_builddir)/tools/circe1_plot
	echo 1 0.001 1.0 50 -1.0 11 11 500.0 2 $(CIRCE1_VER) $(CIRCE1_REV) | $(CPLOT)

dg-tesla.dat: $(top_builddir)/tools/circe1_plot
	echo 1 0.001 1.0 50 -1.0 22 11 500.0 2 $(CIRCE1_VER) $(CIRCE1_REV) | $(CPLOT)

de-xband.dat: $(top_builddir)/tools/circe1_plot
	echo 1 0.001 1.0 50 -1.0 11 11 500.0 3 $(CIRCE1_VER) $(CIRCE1_REV) | $(CPLOT)

dg-xband.dat: $(top_builddir)/tools/circe1_plot
	echo 1 0.001 1.0 50 -1.0 22 11 500.0 3 $(CIRCE1_VER) $(CIRCE1_REV) | $(CPLOT)

de-sbandt.dat: $(top_builddir)/tools/circe1_plot
	echo 1 0.001 1.0 50 -1.0 11 11 1000.0 1 $(CIRCE1_VER) $(CIRCE1_REV) | $(CPLOT)

dg-sbandt.dat: $(top_builddir)/tools/circe1_plot
	echo 1 0.001 1.0 50 -1.0 22 11 1000.0 1 $(CIRCE1_VER) $(CIRCE1_REV) | $(CPLOT)

de-teslat.dat: $(top_builddir)/tools/circe1_plot
	echo 1 0.001 1.0 50 -1.0 11 11 1000.0 2 $(CIRCE1_VER) $(CIRCE1_REV) | $(CPLOT)

dg-teslat.dat: $(top_builddir)/tools/circe1_plot
	echo 1 0.001 1.0 50 -1.0 22 11 1000.0 2 $(CIRCE1_VER) $(CIRCE1_REV) | $(CPLOT)

de-xbandt.dat: $(top_builddir)/tools/circe1_plot
	echo 1 0.001 1.0 50 -1.0 11 11 1000.0 3 $(CIRCE1_VER) $(CIRCE1_REV) | $(CPLOT)

dg-xbandt.dat: $(top_builddir)/tools/circe1_plot
	echo 1 0.001 1.0 50 -1.0 22 11 1000.0 3 $(CIRCE1_VER) $(CIRCE1_REV) | $(CPLOT)

de-tesla3.dat: $(top_builddir)/tools/circe1_plot
	echo 1 0.001 1.0 50 -1.0 11 11 350.0 2 $(CIRCE1_VER) $(CIRCE1_REV) | $(CPLOT)

dg-tesla3.dat: $(top_builddir)/tools/circe1_plot
	echo 1 0.001 1.0 50 -1.0 22 11 350.0 2 $(CIRCE1_VER) $(CIRCE1_REV) | $(CPLOT)

de-tesla8.dat: $(top_builddir)/tools/circe1_plot
	echo 1 0.001 1.0 50 -1.0 11 11 800.0 2 $(CIRCE1_VER) $(CIRCE1_REV) | $(CPLOT)

dg-tesla8.dat: $(top_builddir)/tools/circe1_plot
	echo 1 0.001 1.0 50 -1.0 22 11 800.0 2 $(CIRCE1_VER) $(CIRCE1_REV) | $(CPLOT)

de-sband-ee.dat: $(top_builddir)/tools/circe1_plot
	echo 1 0.001 1.0 50 -1.0 11 11 500.0 4 $(CIRCE1_VER) $(CIRCE1_REV) | $(CPLOT)

dg-sband-ee.dat: $(top_builddir)/tools/circe1_plot
	echo 1 0.001 1.0 50 -1.0 22 11 500.0 4 $(CIRCE1_VER) $(CIRCE1_REV) | $(CPLOT)

de-tesla-ee.dat: $(top_builddir)/tools/circe1_plot
	echo 1 0.001 1.0 50 -1.0 11 11 500.0 5 $(CIRCE1_VER) $(CIRCE1_REV) | $(CPLOT)

dg-tesla-ee.dat: $(top_builddir)/tools/circe1_plot
	echo 1 0.001 1.0 50 -1.0 22 11 500.0 5 $(CIRCE1_VER) $(CIRCE1_REV) | $(CPLOT)

de-xband-ee.dat: $(top_builddir)/tools/circe1_plot
	echo 1 0.001 1.0 50 -1.0 11 11 500.0 6 $(CIRCE1_VER) $(CIRCE1_REV) | $(CPLOT)

dg-xband-ee.dat: $(top_builddir)/tools/circe1_plot
	echo 1 0.001 1.0 50 -1.0 22 11 500.0 6 $(CIRCE1_VER) $(CIRCE1_REV) | $(CPLOT)

de-sbandt-ee.dat: $(top_builddir)/tools/circe1_plot
	echo 1 0.001 1.0 50 -1.0 11 11 1000.0 4 $(CIRCE1_VER) $(CIRCE1_REV) | $(CPLOT)

dg-sbandt-ee.dat: $(top_builddir)/tools/circe1_plot
	echo 1 0.001 1.0 50 -1.0 22 11 1000.0 4 $(CIRCE1_VER) $(CIRCE1_REV) | $(CPLOT)

de-teslat-ee.dat: $(top_builddir)/tools/circe1_plot
	echo 1 0.001 1.0 50 -1.0 11 11 1000.0 5 $(CIRCE1_VER) $(CIRCE1_REV) | $(CPLOT)

dg-teslat-ee.dat: $(top_builddir)/tools/circe1_plot
	echo 1 0.001 1.0 50 -1.0 22 11 1000.0 5 $(CIRCE1_VER) $(CIRCE1_REV) | $(CPLOT)

de-xbandt-ee.dat: $(top_builddir)/tools/circe1_plot
	echo 1 0.001 1.0 50 -1.0 11 11 1000.0 6 $(CIRCE1_VER) $(CIRCE1_REV) | $(CPLOT)

dg-xbandt-ee.dat: $(top_builddir)/tools/circe1_plot
	echo 1 0.001 1.0 50 -1.0 22 11 1000.0 6 $(CIRCE1_VER) $(CIRCE1_REV) | $(CPLOT)

CPLOT = $(top_builddir)/tools/circe1_plot | $(GREP) -v circe1: | $(SED) 's/E/e/g' | $(CPIF) $@

tex-comments: tex-comments.sh
	cp $< $@
	chmod +x $@

SUFFIXES = .tex .pdf

if DISTRIBUTION
if PDFLATEX_AVAILABLE
if MPOST_AVAILABLE
if SUPP_PDF_AVAILABLE
if MAKEINDEX_AVAILABLE
.tex.pdf:
	-TEXINPUTS=$(TEX_FLAGS) $(PDFLATEX) $<
	$(MAKEINDEX) -o $*.ind $*.idx
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

if DISTRIBUTION
if SUPP_PDF_AVAILABLE
if PDFLATEX_AVAILABLE
circe1.pdf: $(PICTURES_PDF) dist.1
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
		$(PICTURES_PDF) tex-comments circe1_plot *.dat dist.mp \
		fit.mp dist.11 dist.12 dist.13 dist.14 
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

########################################################################
## The End.
########################################################################

