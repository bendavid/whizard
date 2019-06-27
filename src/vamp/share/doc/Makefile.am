# Makefile.am --
# $Id: Makefile.am 317 2010-04-18 00:31:03Z ohl $
########################################################################

WEBS = \
  prelude.nw \
  divisions.nw vamp.nw \
  vampi.nw \
  vamp_test.nw vamp_test0.nw application.nw \
  vamp_kinds.nw constants.nw exceptions.nw \
  tao_random_numbers.nw specfun.nw vamp_stat.nw histograms.nw \
  utils.nw linalg.nw products.nw kinematics.nw coordinates.nw \
  mpi90.nw \
  pmep.nw \
  postlude.nw

PDFS = vamp.pdf preview.pdf preview2.pdf

LATEX_STYLES = \
    feynmp.sty feynmp.mp \
    noweb.sty emp.sty flex.cls thohacks.sty thophys.sty

EXTRA_DIST = \
  tex-comments.sh \
  vegas.d vamp.d \
  $(LATEX_STYLES)

dist_doc_DATA = $(PDFS)

if NOWEB_AVAILABLE
pdf-local: vamp.pdf preview.pdf preview2.pdf
else
pdf-local: preview.pdf preview2.pdf
endif

VPATH = $(srcdir):$(top_builddir)/src:$(top_srcdir)/src

if NOWEB_AVAILABLE

vamp.tex: $(WEBS)
	$(NOWEAVE) -filter ./tex-comments -delay -index \
	    `for i in  $^; do case $$i in *.nw) echo $$i;; esac done` \
	  | $(CPIF) $@

vamp.tex: tex-comments

endif NOWEB_AVAILABLE

tex-comments: tex-comments.sh
	cp $< $@
	chmod +x $@

preview.pdf: vegas.data vamp.data

vegas.data: vegas.d
	cp $< $@

vamp.data: vamp.d
	cp $< $@

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
	-rm -f *.data *.mpx *.[1-9] *.t[1-9] vamp*.mp preview*.mp \
	  *.out *.log *.aux *.idx *.ilg *.ind *.rcs *.toc \
	  tex-comments vamp.tex
	-test "$(srcdir)" != "." && rm -f vamp.pdf \
		preview.pdf preview2.pdf
clean-latex:
maintainer-clean-latex:
	-rm -f vamp.pdf preview.pdf preview2.pdf
if NOWEB_AVAILABLE
mostlyclean-vamp:
	-test "$(srcdir)" != "." && rm -f vamp.pdf \
		preview.pdf preview2.pdf
maintainer-clean-vamp:
else
mostlyclean-vamp:
maintainer-clean-vamp:
endif
.PHONY: mostlyclean-latex clean-latex maintainer-clean-latex
.PHONY: mostlyclean-vamp maintainer-clean-vamp

## Remove backup files
maintainer-clean-backup:
	-rm -f *~
.PHONY: maintainer-clean-backup

## Register additional clean targets
mostlyclean-local: mostlyclean-latex mostlyclean-vamp
clean-local: clean-latex
maintainer-clean-local: maintainer-clean-latex maintainer-clean-vamp \
	maintainer-clean-backup

distclean-local:
	-test "$(srcdir)" != "." && rm -f $(LATEX_STYLES)

########################################################################
## The End.
########################################################################

