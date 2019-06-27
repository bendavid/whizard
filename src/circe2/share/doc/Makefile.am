# Makefile.am --
# $Id: Makefile.am 317 2010-04-18 00:31:03Z ohl $
########################################################################

include $(top_srcdir)/src/Makefile.sources

WEBS = \
  prelude.nw circe2.nw postlude.nw

PDFS = circe2.pdf 

LATEX_STYLES = \
    emp.sty noweb.sty thohacks.sty thopp.sty ocamlweb.sty

EXTRA_DIST = \
  tex-comments.sh \
  $(LATEX_STYLES)

dist_doc_DATA = $(PDFS)

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

MPOST_LATEX = TEX=$(LATEX) $(MPOST)

if !OCAMLWEB_AVAILABLE
circe2.pdf:
else
circe2.pdf: $(CIRCE2TOOLLIB_INTERFACE) $(CIRCE2TOOLLIB_IMPLEMENTATION) \
		$(CIRCE2TOOL_IMPLEMENTATION) index.tex
endif

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

distclean-local:
	-test "$(srcdir)" != "." && rm -f $(LATEX_STYLES)

########################################################################
## The End.
########################################################################

