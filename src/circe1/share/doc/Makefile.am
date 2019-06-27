# Makefile.am --
# $Id: Makefile.am 317 2010-04-18 00:31:03Z ohl $
########################################################################

WEBS = \
  prelude.nw \
  circe1.nw minuit.nw postlude.nw

PDFS = circe1.pdf 

LATEX_STYLES = \
    noweb.sty thohacks.sty thopp.sty

EXTRA_DIST = \
  tex-comments.sh \
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
endif
endif
endif

## Cleanup tasks
mostlyclean-latex:
	-rm -f *.data *.rcs *.mpx *.[1-9] *.t[1-9] circe*.mp preview*.mp \
		circe1.tex *.out *.log *.aux *.idx *.ilg *.ind *.toc \
		tex-comments
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

distclean-local:
	-test "$(srcdir)" != "." && rm -f $(LATEX_STYLES)

########################################################################
## The End.
########################################################################

