# Makefile.am --
# $Id: Makefile.am 319 2010-04-18 15:00:54Z ohl $
##
## Process this file with automake to produce Makefile.in
##
########################################################################
#
# Copyright (C) 1999-2011 by 
#     Wolfgang Kilian <kilian@physik.uni-siegen.de>
#     Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
#     Juergen Reuter <juergen.reuter@desy.de>
#     Christian Speckner <christian.speckner@physik.uni-freiburg.de>
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

SOURCE_FILES = \
  circe2.f 

EXTRA_SOURCE_FILES = \
  taorng.f \
  sample_taorng.f

MODULE_FILES = \
  circe2.$(FC_MODULE_EXT) \
  taorng.$(FC_MODULE_EXT) 
#  kinds.$(FC_MODULE_EXT)

NOWEB_FILES = \
    prelude.nw postlude.nw circe2.nw

EXTRA_DIST = $(NOWEB_FILES) $(SOURCE_FILES) $(CIRCE2_CAML) \
	$(EXTRA_SOURCE_FILES)

# Fortran90 module files are generated at the same time as object files
.lo.$(FC_MODULE_EXT):
	@:
#	touch $@

lib_LTLIBRARIES = libcirce2.la
libcirce2_la_SOURCES = $(SOURCE_FILES)
# There are no module files for the FORTRAN 77 code
# pkginclude_DATA = $(MODULE_FILES)

AM_FFLAGS = 
AM_FCFLAGS = 

########################################################################
## Default Fortran compiler options

## Profiling
if FC_USE_PROFILING
AM_FFLAGS += $(FCFLAGS_PROFILING)
AM_FCFLAGS += $(FCFLAGS_PROFILING)
endif

## OpenMP
if FC_USE_OPENMP
AM_FFLAGS += $(FCFLAGS_OPENMP)
AM_FCFLAGS += $(FCFLAGS_OPENMP)
endif

########################################################################
# noweb
########################################################################

TRIPLE = $(srcdir)/prelude.nw $< $(srcdir)/postlude.nw
WEBS = $(srcdir)/prelude.nw $(srcdir)/circe2.nw $(srcdir)/postlude.nw
FTANGLE = sed 's/^[     ]*<</<</' | $(NOTANGLE)
NOTANGLE_IT = \
  cat $(TRIPLE) | $(FTANGLE) -R'[[$@]]' | $(CPIF) $@

SUFFIXES = .nw 

.nw.f:
	$(NOTANGLE_IT)

taorng.f: postlude.nw
	cat $(WEBS) | $(FTANGLE) -R'[[$@]]' | $(CPIF) $@

sample_taorng.f: postlude.nw
	cat $(WEBS) | $(FTANGLE) -R'[[$@]]' | $(CPIF) $@

########################################################################
# The following line just says
#    include Makefile.depend
# but in a portable fashion (depending on automake's AM_MAKE_INCLUDE
########################################################################
@am__include@ @am__quote@Makefile.depend@am__quote@

Makefile.depend: $(SOURCE_FILES)
	@rm -f $@
	for src in $^; do \
          module="`basename $$src | sed 's/\.f//'`"; \
	  grep '^ *use ' $$src \
	    | grep -v '!NODEP!' \
	    | sed -e 's/^ *use */'$$module'.lo: /' \
	          -e 's/, *only:.*//' \
	          -e 's/, *&//' \
	          -e 's/, *.*=>.*//' \
	          -e 's/ *$$/.lo/' \
				 $(depend_filter_extra) ; \
	done > $@

DISTCLEANFILES = Makefile.depend

if OCAML_AVAILABLE

@am__include@ @am__quote@Makefile.depend_ocaml@am__quote@

PARSERS = cascade comphep vertex model_file

Makefile.depend_ocaml: $(CIRCE2_CAML)
	@rm -f $@
	$(OCAMLDEP) -I $(srcdir) $^ $(OMEGA_DERIVED_CAML) \
	  | sed 's,[^ 	]*/,,g' > $@

DISTCLEANFILES += Makefile.depend_ocaml

endif OCAML_AVAILABLE

clean-local:
	rm -f *.$(FC_MODULE_EXT) *.g90


###           module="`basename $$src | sed 's/\.f[90][0358]//'`"; 
########################################################################
# MPI
########################################################################
###
### # The -mismatch_all is for mpi_send() etc.
### MPIFC = mpif90
### MPIFCFLAGS = # -mismatch_all


### ### OLD Makefile from ThO's CIRCE2 distribution
###    JRR removes those things which have been transferred to the new
###	automake structure or are irrelevant for it
### 
### # $Id: Makefile 172 2010-05-21 12:58:31Z ohl $
### ########################################################################
### 
### GV_OK = yes
### 
### HTML_DIR = $(HOME)/public_html/circe2
### 
### IO = binary
### # IO = ascii
### 
### MODE = unweighted
### # MODE = weighted
### 
### # OPTIONS = triangle
### # OPTIONS = triangle bins = 20  map = id { 50 [0.75,0.85] } map = id { 5 [0.85,1] }
### OPTIONS = bins = 10  map = id { 40 [0.75,1] }
### 
### EVENTS = foo
### NEVENTS = 1000000
### 
### ########################################################################
### 
### CC = gcc
### CFLAGS = -Wall -O2 -mieee-fp
### 
### FC = g77
### FFLAGS = -Wall -O2 -mieee-fp
### 
### STREAM_PARSER = -pp camlp4o
### 
### HALFWIDTH = 14.8cm
### GHOSTVIEW = gv
### 
### HEVEAOPTS = -nosymb -exec xxdate.exe article.hva fancysection.hva mathaccents.hva
### CUSTOM_HEVEA = custom.hva
### HACHAOPTS = -tocbis
### 
### ########################################################################
### 
### LIB_SRC = circe2.f
### LIB_OBJ = $(LIB_SRC:.f=.o)
### 
### LIBD_SRC = circe2d.f
### LIBD_OBJ = $(LIBD_SRC:.f=.o)
### 
### LIB_SRC_C = circe2_c.c
### LIB_OBJ_C = $(LIB_SRC_C:.c=.o)
### 
### LIB_SRC_CPP = circe2_cpp.c
### LIB_OBJ_CPP = $(LIB_SRC_CPP:.cpp=.o)
### 
### SAMPLE_SRC = sample.f circe2ls.f
### SAMPLE_OBJ = $(SAMPLE_SRC:.f=.o)
### SAMPLE_BIN = $(SAMPLE_SRC:.f=)
### 
### XTRA_SRC = write3_ascii.f write3_binary.c
### XTRA_OBJ = write3_ascii.o write3_binary.o
### 
### TARS = Makefile $(LIB_SRC) $(XTRA_SRC) $(SAMPLES_SRC) $(WEBS) \
### 	$(TOOLLIB_SRC) $(TOOL_SRC) circe2.ps .depend
### 
### all: lib
### world: samples opt ps manual.ps
### samples: sample sample_binary circe2ls
### lib: libcirce2.a libcirce2d.a
### 
### opt: circe2_tool
### bin: circe2_tool.bin
### 
### ps: circe2.ps
### psv: circe2.psv
### dvi: circe2.dvi
### pdf: manual.pdf
### pdfv: manual.pdfv
### 
### www: index.html
### 
### ########################################################################
### 
### libcirce2.a: $(LIB_OBJ)
### 	ar cr $@  $(LIB_OBJ)
### 	ranlib $@
### 
### libcirce2_c.a: $(LIB_OBJ_C)
### 	ar cr $@  $(LIB_OBJ_C)
### 	ranlib $@
### 
### libcirce2_cpp.a: $(LIB_OBJ_CPP)
### 	ar cr $@  $(LIB_OBJ_CPP)
### 	ranlib $@
### 
### libcirce2d.a: $(LIBD_OBJ)
### 	ar cr $@  $(LIBD_OBJ)
### 	ranlib $@
### 
### sample: sample.o write3_ascii.o taorng.o libcirce2.a
### 	$(FC) $(FFLAGS) -o $@ sample.o write3_ascii.o taorng.o -L. -lcirce2
### 
### sample_binary: sample.o write3_binary.o taorng.o libcirce2.a
### 	$(FC) $(FFLAGS) -o $@ sample.o write3_binary.o taorng.o -L. -lcirce2
### 
### %.o: %.f
### 	$(FC) $(FFLAGS) -c $<
### 
### %.o: %.c
### 	$(CC) $(CFLAGS) -c $<
### 
### %.o: %.cpp
### 	$(CPP) $(CFLAGS) -c $<
### 
### sample.o: sample.f
### write3_ascii.o: write3_ascii.f
### write3_binary.o: write3_binary.c
### taorng.o: taorng.f
### 
### circe2.f: $(WEBS)
### 	cat $(WEBS) | $(FTANGLE) -R'[[$@]]' | $(CPIF) $@
### 
### circe2_c.c: $(WEBS)
### 	cat $(WEBS) | $(FTANGLE) -R'[[$@]]' | $(CPIF) $@
### 
### circe2_cpp.cpp: $(WEBS)
### 	cat $(WEBS) | $(FTANGLE) -R'[[$@]]' | $(CPIF) $@
### 
### circe2d.f: $(WEBS)
### 	cat $(WEBS) | $(FTANGLE) -R'[[$@]]' | $(CPIF) $@
### 
### sample.f: $(WEBS)
### 	cat $(WEBS) | $(FTANGLE) -R'[[$@]]' | $(CPIF) $@
### 
### write3_ascii.f: $(WEBS)
### 	cat $(WEBS) | $(FTANGLE) -R'[[$@]]' | $(CPIF) $@
### 
### write3_binary.c: $(WEBS)
### 	cat $(WEBS) | $(TANGLE) -R'[[$@]]' | $(CPIF) $@
### 
### circe2ls.f: $(WEBS)
### 	cat $(WEBS) | $(FTANGLE) -R'[[$@]]' | $(CPIF) $@
### 
### ########################################################################
### 
### manual.tex: prelude.nw 
### 	($(WEAVE) -delay prelude.nw \
### 	  | sed -e '/documentclass/s/a4paper/12pt,a4paper/' \
###                 -e '/usepackage.*thopp/s/^/%%%/'; \
###          echo '%BEGIN IMAGE'; \
###          echo '\end{empfile}'; \
###          echo '%END IMAGE'; \
###          echo '\end{document}'; ) | $(CPIF) $@
### 
### circe2.tex: $(WEBS)
### 	$(WEAVE) -delay -index $(WEBS) | $(CPIF) $@
### 
### circe2.dvi: $(TOOLLIB_INTERFACE) $(TOOLLIB_IMPLEMENTATION) \
### 		$(TOOL_IMPLEMENTATION) index.tex
### 
### index.html: manual.html
### 	$(HACHA) $(HACHAOPTS) -o $@ $<
### 
### %.html: %.tex
### 	$(HEVEA) $(HEVEAOPTS) $(CUSTOM_HEVEA) $<
### #	$(METAPOST) $*pics.mp
### 	$(IMAGEN) $*
### 	TEX=latex $(METAPOST) $*.image.mp
### 	$(IMAGEN) $*
### 	$(HEVEA) -fix $(HEVEAOPTS) $(CUSTOM_HEVEA) $<
### 
### ########################################################################
### 
### circe2_tool: $(TOOLLIB_CMX) $(TOOL_CMX)
### 	$(OCAMLOPT) $(OCAMLFLAGS) $(OCAMLOPTFLAGS) -o $@ \
### 		unix.cmxa bigarray.cmxa $(TOOLLIB_CMX) $(TOOL_CMX)
### 
### circe2_tool.bin: $(TOOLLIB_CMO) $(TOOL_CMO)
### 	$(OCAMLC) -custom $(OCAMLFLAGS) -o $@ \
### 		unix.cma bigarray.cma $(TOOLLIB_CMO) $(TOOL_CMO)
### 
### circe2.top: $(TOOLLIB_CMO)
### 	$(OCAMLMKTOP) $(OCAMLFLAGS) -o $@ \
### 		unix.cma bigarray.cma $(TOOLLIB_CMO)
### 
### %.cmo: %.ml
### 	@rm -f $@
### 	$(OCAMLC) $(OCAMLFLAGS) -c $<
### 
### %.cmx: %.ml
### 	@rm -f $@
### 	$(OCAMLOPT) $(OCAMLFLAGS) $(OCAMLOPTFLAGS) -c $<
### 
### %.cmi: %.mli
### 	@rm -f $*.cmi
### 	$(OCAMLCI) $(OCAMLFLAGS) -c $<
### 
### 
### ########################################################################
### 
### %.dvi: %.tex
### 	-$(LATEX) $<
### 	makeindex -o $*.ind $*.idx
### 	TEX=$(LATEX) $(METAPOST) $*.mp
### 	$(LATEX) $<
### 	if grep -s 'Rerun to get cross-references right.' $*.log; then \
### 	  $(LATEX) $<; \
### 	fi
### 
### %.pdf: %.tex
### 	@rm -f $*.aux
### 	-$(PDFLATEX) $<
### 	makeindex -o $*.ind $*.idx
### 	TEX=$(LATEX) $(METAPOST) $*.mp
### 	$(PDFLATEX) $<
### 	if grep -s 'Rerun to get cross-references right.' $*.log; then \
### 	  $(PDFLATEX) $<; \
### 	fi
### 
### %.dvj: %.dvi
### #	dvidvi -m '2:0,1($(HALFWIDTH),0)' $< $@
### 	dvitodvi '2:0+1($(HALFWIDTH),0)' $< > $@
### 	@echo
### 
### %.lpr: %.dvj
### 	$(DVIPS) -t landscape -t a4 -o $@ $<
### 
### ifeq ($(GV_OK),yes)
### 
### %.ps: %.dvj
### 	$(DVIPS) -t landscape -t a4 -o $@ $<
### 
### %.psv: %.ps
### 	nohup sh -c '$(GHOSTVIEW) -media a4 -landscape -swap $< &' 2>/dev/null 1>/dev/null
### 
### else
### 
### %.ps: %.dvj
### 	$(DVIPS) -t a4r -o $@ $<
### 
### %.psv: %.ps
### 	nohup sh -c '$(GHOSTVIEW) -media a4r -portrait $< &' 2>/dev/null 1>/dev/null
### 
### endif
### 
### %.pdfv: %.pdf
### 	acroread $<
### 
### manual.ps: manual.dvi
### 	$(DVIPS) -t a4 -o $@ $<
### 
### manual.psv: manual.ps
### 	nohup sh -c '$(GHOSTVIEW) -media a4 -portrait $< &' 2>/dev/null 1>/dev/null
### 
### clean:
### 	rm -f *~ *.o *.tex *.toc *.aux *.dv[ij] *.log *.rcs \
### 		*.mp *.mpx circe2.[0-9]* manual.[0-9]* *.idx *.ilg *.ind \
### 		*.implementation *.interface *.cm[iox] \
### 		*.new.* *.new-w.* manual.out *.lpr
### 
### realclean: clean
### 	rm -f sample* circe2_tool \
### 		*.aux *.ind *.bin *.top *.a *.f *.c *.ps circe2.tar.gz \
### 		manual.pdf
### 
### tar: circe2.tar.gz
### circe2.tar.gz: $(TARS)
### 	tar cf - $(TARS) | gzip -9 >$@
### 
### ########################################################################
### 
### demo%.f: manual.tex
### 	 sed -n -e '/^      program demo$*$$/,/^      end$$/p' \
### 		-e '/^      subroutine random/,/^      end$$/p' $< >$@
### 
### DEMOS = demo1 demo2 demo3 demo4 demo5 demo6 demo7 demo8 demo9
### .PHONY: demos
### demos: $(DEMOS)
### 
### demo%: demo%.f
### 	$(FC) $(FFLAGS) -o $@ $< -L. -lcirce2
### 
### export: indent circe2.ps manual.pdf manual.ps $(DEMOS:=.f)
### 	scp -p circe2.f heplix:/usr/local/ftp/pub/ohl/circe2
### 	gzip -9 <circe2.ps >circe2.ps.gz
### 	gzip -9 <manual.ps >manual.ps.gz
### 	scp -p circe2.ps.gz manual.ps.gz manual.pdf \
### 		heplix:/usr/local/ftp/pub/ohl/circe2/doc
### 	scp -p circe2ls.f $(DEMOS:=.f) \
### 		heplix:/usr/local/ftp/pub/ohl/circe2/demos
### 
### export.www: www
### 	install -m 444 *_motif.gif manual*.gif index.html manual?*.html \
### 		$(HTML_DIR)
### 
### ########################################################################
### 
### .PHONY: indent
### indent: circe2.f circe2d.f circe2ls.f sample.f write3_ascii.f
### 	for f in $<; do \
### 	  emacs -q -batch $$f \
### 	    -eval '(progn \
### 		     (fortran-mode) \
### 		     (mark-whole-buffer) \
###                      (indent-region (point-min) (point-max) nil) \
###                      (save-buffer))'; \
### 	done
### 
### CHOP_CIRCULAR = \
### 	perl -pe '\
### 		if (s/\\$$//) { chomp; $$_.=<>; } \
### 		($$t) = split (/:/); \
### 		s/ $$t//g; \
### 		s/ +/ /g;'
### 
### .PHONY: depend
### depend:
### 	$(OCAMLDEP) *.mli *.ml | $(CHOP_CIRCULAR) >.depend
### 
### include .depend
### ########################################################################
### 
### .PHONY: test
### 
### test: xy.histo
### 	./circe2_tool -c '\
###           { file = "$(EVENTS).circe" \
###             { design = "*" roots = 500 comment = "Circe2 trivial self test" \
###               { pid = gamma events = "$(EVENTS).map" binary $(OPTIONS) } } }'
### ifeq ($(IO),binary)
### 	echo "'$(EVENTS).circe' '*' 500 22 0 22 0 'unweighted' $(NEVENTS)" \
### 		| ./sample_binary > $(EVENTS).new.map
### 	echo "'$(EVENTS).circe' '*' 500 22 0 22 0 'weighted' $(NEVENTS)" \
### 		| ./sample_binary > $(EVENTS).new-w.map
### else
### 	echo "'$(EVENTS).circe' '*' 500 22 0 22 0 'unweighted' $(NEVENTS)" \
### 		| ./sample | ./circe2_tool -ob $(EVENTS).new.map
### 	echo "'$(EVENTS).circe' '*' 500 22 0 22 0 'weighted' $(NEVENTS)" \
### 		| ./sample | ./circe2_tool -ob $(EVENTS).new-w.map
### endif
### 	./circe2_tool -hb $(EVENTS).new.map -p .new.histo
### 	./circe2_tool -hb $(EVENTS).new-w.map -p .new-w.histo
### 
### xy.histo: $(EVENTS).map
### 	./circe2_tool -hb $< -p .histo
### 
### %.map: %.events
### 	./circe2_tool -ia $< -ob $@
### 
### ########################################################################
### 
### test-block-data:
### 	./circe2_tool -c '\
###           { file = "foo.circe" block_data = "foo.f" \
###             { design = "*" roots = 500 \
###               { pid = gamma events = "foo.map" binary bins = 50 iterations = 1 } } }'
### 
### test-parser1:
### 	./circe2_tool -c '\
###           { file = "foo.circe" \
###             { design = "*" roots = 500 \
###               { pid = gamma events = "foo.1" binary \
###                 bins/1 = 1 bins/2 = 20 \
###                 map/1 = id { 1 [0.2,0.4] } \
###                 map/1 = id { 1 [0.5,0.6] } } } }'
### 
### test-parser2:
### 	./circe2_tool -c '\
###           { file = "foo.circe" \
###             { design = "*" roots = 500 \
###               { pid = gamma events = "foo.1" binary \
###                 bins/1 = 10 \
###                 map/1 = id { 10 [0.2,0.4] } \
###                 map/1 = power { 10 [0.5,0.6] beta = -0.3 eta = 0 } \
###                 bins/2 = 20 } } }'
### 
### test-parser3:
### 	./circe2_tool -c '\
###           { file = "foo.circe" \
###             { design = "*" roots = 500 \
###               { pid = gamma events = "foo.1" binary \
###                 bins/1 = 10 \
###                 map/1 = id { 10 [0.2,0.4] } \
###                 map/1 = power { 10 [0.5,0.6] beta = -1.3 eta = 0 } \
###                 bins/2 = 20 } } }'
### 
### test-parser4:
### 	./circe2_tool -c '\
###           { file = "foo.circe" \
###             { design = "*" roots = 500 \
###               { pid = gamma events = "foo.1" binary \
###                 bins/1 = 1 \
###                 map/1 = id { 1 [0.2,0.4] } \
###                 map/1 = resonance { 1 [0.5,0.6] center = 0 width = 1 } \
###                 bins/2 = 20 } } }'
### 
### big-test:
### 	./circe2_tool -c '\
###           { file = "foo.circe" \
###             { design = "test/bins=10" roots = 500 \
###               { pid = gamma events = "foo.map" binary bins = 10 $(OPTIONS) } } \
###             { design = "test/bins=20" roots = 500 \
###               { pid = gamma events = "foo.map" binary bins = 20 $(OPTIONS) } } \
###             { design = "test/bins=50" roots = 500 \
###               { pid = gamma events = "foo.map" binary bins = 50 $(OPTIONS) } } \
###             { design = "test/bins=100" roots = 500 \
###               { pid = gamma events = "foo.map" binary bins = 100 $(OPTIONS) } } \
###             { design = "test/bins=10/t" roots = 500 \
###               { pid = gamma events = "foo.map" binary bins = 10 triangle $(OPTIONS) } } \
###             { design = "test/bins=20/t" roots = 500 \
###               { pid = gamma events = "foo.map" binary bins = 20 triangle $(OPTIONS) } } \
###             { design = "test/bins=50/t" roots = 500 \
###               { pid = gamma events = "foo.map" binary bins = 50 triangle $(OPTIONS) } } \
###             { design = "test/bins=100/t" roots = 500 \
###               { pid = gamma events = "foo.map" binary bins = 100 triangle $(OPTIONS) } } }'
### 
### ########################################################################
### 
### NBINS = 50
### 
### daniel: $(TOOLLIB_CMX) daniel.cmx
### 	$(OCAMLOPT) $(OCAMLOPTFLAGS) -o $@ unix.cmxa bigarray.cmxa $^
### 
### valery: valery.ml
### 	$(OCAMLOPT) $(OCAMLOPTFLAGS) -o $@ unix.cmxa bigarray.cmxa $<
### 
### valery-data-boot:
### 	zcat valery_data/fort.1.gz | tail -c +5 >teslagg_200.events
### 	zcat valery_data/fort.2.gz | tail -c +5 >teslagg_500.events
### 	zcat valery_data/fort.3.gz | tail -c +5 >teslagg_800.events
### 	zcat valery_data/fort.4.gz | tail -c +5 >teslage_500.events
### 
### valery-data-split:
### 	./valery -r 200 -D 'TESLA/GG' -b $(NBINS) -d teslagg_200 \
### 		-o teslagg_200.circe >teslagg_200.commands
### 	./valery -r 500 -D 'TESLA/GG' -b $(NBINS) -d teslagg_500 \
### 		-o teslagg_500.circe >teslagg_500.commands
### 	./valery -r 800 -D 'TESLA/GG' -b $(NBINS) -d teslagg_800 \
### 		-o teslagg_800.circe >teslagg_800.commands
### 	./valery -r 500 -D 'TESLA/GE' -b $(NBINS) -d teslage_500 \
### 		-o teslage_500.circe >teslage_500.commands
### 
### valery-data-split-polavg:
### 	./valery -polavg -r 200 -D 'TESLA/GG' -b $(NBINS) -d teslagg_200 \
### 		-o teslagg_200_polavg.circe >teslagg_200_polavg.commands
### 	./valery -polavg -r 500 -D 'TESLA/GG' -b $(NBINS) -d teslagg_500 \
### 		-o teslagg_500_polavg.circe >teslagg_500_polavg.commands
### 	./valery -polavg -r 800 -D 'TESLA/GG' -b $(NBINS) -d teslagg_800 \
### 		-o teslagg_800_polavg.circe >teslagg_800_polavg.commands
### 	./valery -polavg -r 500 -D 'TESLA/GE' -b $(NBINS) -d teslage_500 \
### 		-o teslage_500_polavg.circe >teslage_500_polavg.commands
### 
### valery-data-commands:
### 	./valery -n -r 200 -D 'TESLA/GG' -b $(NBINS) -d teslagg_200 \
### 		-o teslagg_200.circe >teslagg_200.commands
### 	./valery -n -r 500 -D 'TESLA/GG' -b $(NBINS) -d teslagg_500 \
### 		-o teslagg_500.circe >teslagg_500.commands
### 	./valery -n -r 800 -D 'TESLA/GG' -b $(NBINS) -d teslagg_800 \
### 		-o teslagg_800.circe >teslagg_800.commands
### 	./valery -n -r 500 -D 'TESLA/GE' -b $(NBINS) -d teslage_500 \
### 		-o teslage_500.circe >teslage_500.commands
### 
### valery-data-commands-polavg:
### 	./valery -n -polavg -r 200 -D 'TESLA/GG' -b $(NBINS) -d teslagg_200 \
### 		-o teslagg_200_polavg.circe >teslagg_200_polavg.commands
### 	./valery -n -polavg -r 500 -D 'TESLA/GG' -b $(NBINS) -d teslagg_500 \
### 		-o teslagg_500_polavg.circe >teslagg_500_polavg.commands
### 	./valery -n -polavg -r 800 -D 'TESLA/GG' -b $(NBINS) -d teslagg_800 \
### 		-o teslagg_800_polavg.circe >teslagg_800_polavg.commands
### 	./valery -n -polavg -r 500 -D 'TESLA/GE' -b $(NBINS) -d teslage_500 \
### 		-o teslage_500_polavg.circe >teslage_500_polavg.commands
### 
### valery-data-circe2:
### 	./circe2_tool -f teslagg_200.commands
### 	./circe2_tool -f teslagg_500.commands
### 	./circe2_tool -f teslagg_800.commands
### 	./circe2_tool -f teslage_500.commands
### 
### valery-data-circe2-polavg:
### 	./circe2_tool -f teslagg_200_polavg.commands
### 	./circe2_tool -f teslagg_500_polavg.commands
### 	./circe2_tool -f teslagg_800_polavg.commands
### 	./circe2_tool -f teslage_500_polavg.commands
