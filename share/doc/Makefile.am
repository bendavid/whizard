## Makefile.am -- Makefile for WHIZARD
##
## Process this file with automake to produce Makefile.in

## The manual source has to be distributed
dist_noinst_DATA = manual.tex $(PACKAGE).tex

## The primary targets
if NOWEB_AVAILABLE
dist_pdf_DATA = manual.pdf $(PACKAGE).pdf
else
dist_pdf_DATA = manual.pdf 
endif

ps-local: manual.ps $(PACKAGE).ps
pdf-local: manual.pdf $(PACKAGE).pdf

if HEVEA_AVAILABLE
# html_DATA = manual.html index.html
endif

LATEX_STYLES = \
    noweb.sty thophys.sty

EXTRA_DIST = $(LATEX_STYLES)

## don't try to run the files in parallel (TeXLive 2009 doesn't like it)
manual.ps: $(PACKAGE).ps
manual.pdf: $(PACKAGE).pdf

## Rules for creating PS
.tex.dvi:
	-$(LATEX) $<; \
	while grep 'Rerun to get cross-references right\.' $<.log; \
	do $(LATEX) $<; \
	done
	$(LATEX) $<
.dvi.ps:
	$(DVIPS) $<

## Rules for creating PDF
if PDFLATEX_AVAILABLE 
.tex.pdf: 
	-$(PDFLATEX) $<; \
	while grep 'Rerun to get cross-references right\.' $<.log; \
	do $(PDFLATEX) $<; \
	done
	$(PDFLATEX) $<
else 
if PS2PDF_AVAILABLE
ps.pdf: 
	$(PS2PDF) $<.ps
else
	@echo "Neither pdflatex nor ps2pdf available. The PDF manual cannot be made"
endif 	
endif

## Rules for creating HTML
if HEVEA_AVAILABLE
HEVEAOPTS = -exec xxdate.exe \
	article.hva fancysection.hva mathaccents.hva \
	custom.hva
HACHAOPTS = -tocbis

index.html: manual.html
	$(HACHA) $(HACHAOPTS) -o index.html manual.html

manual.html: manual.tex
	$(HEVEA) $(HEVEAOPTS) manual.tex
	$(IMAGEN) manual
	$(MPOST) manualpics.mp
	$(HEVEA) -fix $(HEVEAOPTS) manual.tex
else	
	@echo "HEVEA not available. The HTML manual cannot be made"
endif

if NOWEB_AVAILABLE
$(PACKAGE).tex: $(top_srcdir)/src/whizard-core/$(PACKAGE).nw
	$(NOWEAVE) -delay $< | $(CPIF) $@
endif

## Cleanup tasks
mostlyclean-latex:
	-rm -f *.aux *.log *.dvi *.toc *.idx *.out \
	manual.ps manual.pdf manual.tex 
clean-latex:
maintainer-clean-latex:
	-rm manual.ps manual.pdf
if NOWEB_AVAILABLE
mostlyclean-whizard:
	-rm -f $(PACKAGE).tex $(PACKAGE).ps $(PACKAGE).pdf
maintainer-clean-whizard:
else
mostlyclean-whizard:
maintainer-clean-whizard:
endif
.PHONY: mostlyclean-latex clean-latex maintainer-clean-latex
.PHONY: mostlyclean-whizard maintainer-clean-whizard

mostlyclean-html:
	-rm -f *.haux
clean-html:
maintainer-clean-html:
	-rm -f manual.html index.html *.gif
.PHONY: mostlyclean-html clean-html

## Remove backup files
maintainer-clean-backup:
	-rm -f *~
.PHONY: maintainer-clean-backup

## Register additional clean targets
mostlyclean-local: mostlyclean-latex mostlyclean-html mostlyclean-whizard
clean-local: clean-latex clean-html
maintainer-clean-local: maintainer-clean-latex maintainer-clean-html \
	maintainer-clean-whizard maintainer-clean-backup

distclean-local:
	-test "$(srcdir)" != "." && rm -f $(LATEX_STYLES)

########################################################################
## The End.
########################################################################
