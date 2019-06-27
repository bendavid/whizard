# Makefile.am --
# $Id: Makefile.am 319 2010-04-18 15:00:54Z ohl $
########################################################################

SOURCE_FILES = \
  circe1.f
#  sample.f

MODULE_FILES = \
  circe1.$(FC_MODULE_EXT) 
#  sample.$(FC_MODULE_EXT) 
#  kinds.$(FC_MODULE_EXT)

NOWEB_FILES = \
    prelude.nw postlude.nw circe1.nw \
    minuit.nw

EXTRA_DIST = $(NOWEB_FILES) $(SOURCE_FILES)

# Fortran90 module files are generated at the same time as object files
.lo.$(FC_MODULE_EXT):
	@:
#	touch $@

pkglib_LTLIBRARIES = libcirce1.la
libcirce1_la_SOURCES = $(SOURCE_FILES)
# There are no module files for the FORTRAN 77 code
# pkginclude_DATA = $(MODULE_FILES)

########################################################################
# noweb
########################################################################

TRIPLE = $(srcdir)/prelude.nw $< $(srcdir)/postlude.nw
FTANGLE = sed 's/^[     ]*<</<</' | $(NOTANGLE)
NOTANGLE_IT = \
  cat $(TRIPLE) | $(FTANGLE) -R'[[$@]]' | $(CPIF) $@

SUFFIXES = .nw 

.nw.f:
	$(NOTANGLE_IT)

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

### OLD Makefile from the CIRCE1 distribution

### # Makefile --
### # $Id: Makefile 66 2002-03-28 17:00:10Z ohl $
### ########################################################################
### 
### V = 1
### R = 3
### # REV = 19960711
### REV = 0
### 
### LDFLAGS = -mieee
### 
### # FC = -f77 -C
### # FC = -CC_f2c='gcc -b i486-linuxaout' f77
### # FC = g77 -L/usr/lib/gcc-lib/i486-linux/egcs-2.91.60 \
### #	 -L/usr/lib/gcc-lib/i486-linux/2.7.2.3
### FC = g77
### 
### FFLAGS = -O2
### 
### CC = gcc
### CPP = $(CC) -E -P -
### CFLAGS = -O2
### 
### TANGLE = notangle
### FTANGLE = sed 's/^[ 	]*<</<</' | $(TANGLE)
### WEAVE = noweave
### CPIF = cpif
### HALFWIDTH = 14.8cm
### 
### # CERNVERSION = new
### CERNVERSION = pro
### # CERNVERSION = old
### CERNLIBDIR = -L/cern/$(CERNVERSION)/lib
### 
### ########################################################################
### 
### TRIPLE = prelude.nw $< postlude.nw
### BRACKET = prelude.nw postlude.nw
### 
### WEBS = prelude.nw circe.nw minuit.nw postlude.nw
### LIBSRCS = circe.f
### LIBOBJS = circe.o
### 
### SAMPLES = circe
### SAMPLES_SRC = $(addsuffix .f, $(addprefix sample_, $(SAMPLES)))
### SAMPLES_BIN = $(addprefix sample_, $(SAMPLES))
### 
### SRCS = $(LIBSRCS) $(SAMPLES_SRC)
### 
### all:  $(LIBOBJS)
### samples: $(SAMPLES_BIN)
### dvi: circe.dvi
### hpv: circe.hpv
### pv: circe.pv
### ps: circe.ps
### 
### libcirce.a: $(LIBOBJS)
### 	ar cr $@  $(LIBOBJS)
### 	ranlib $@
### 
### cplot.f: circe.nw $(BRACKET)
### 	cat $(TRIPLE) | $(FTANGLE) -R'[[$@]]' | $(CPIF) $@
### 
### params.f: circe.nw $(BRACKET)
### 	cat $(TRIPLE) | $(FTANGLE) -R'[[$@]]' | $(CPIF) $@
### 
### sample.f: circe.nw $(BRACKET)
### 	cat $(TRIPLE) | $(FTANGLE) -R'[[$@]]' | $(CPIF) $@
### 
### params.bin: params.o circe.o
### 	$(FC) $(FFLAGS) -o $@ params.o circe.o
### 
### sample.bin: sample.o circe.o
### 	$(FC) $(FFLAGS) -o $@ sample.o circe.o
### 
### sample1.bin: sample1.o circe.o
### 	$(FC) $(FFLAGS) -o $@ sample1.o circe.o
### 
### %.f: %.nw $(BRACKET)
### 	cat $(TRIPLE) | $(FTANGLE) -R'[[$@]]' | $(CPP) | $(CPIF) $@
### 
### %.h: %.nw $(BRACKET)
### 	cat $(TRIPLE) | $(FTANGLE) -R'[[$@]]' | $(CPIF) $@
### 
### %.c: %.nw $(BRACKET)
### 	$(TANGLE) -R'[[$@]]' $(TRIPLE) | $(CPIF) $@
### 
### 
### %.o: %.f
### 	$(FC) $(FFLAGS) -c $<
### 
### %.o: %.c
### 	$(CC) $(CFLAGS) -c $<
### 
### cplot: cplot.o circe.o
### 	$(FC) -o $@ cplot.o circe.o
### 
### circe.tex: $(WEBS)
### 	$(WEAVE) -delay -index $(WEBS) | $(CPIF) $@
### 
### circe.html: $(WEBS)
### 	$(WEAVE) -filter l2h -html -index -delay $(WEBS) | $(CPIF) $@
### 
### manual.tex: prelude.nw postlude.nw
### 	sed -e 's/%def.*$$//'\
### 	    -e '/%!MANUAL%/d' \
### 	    prelude.nw postlude.nw \
### 	  | $(WEAVE) -delay -index | $(CPIF) $@
### 
### circe.dvi manual.dvi: dist.1 fit.11 figures.1
### 
### %.mp: %.mp4 circemacs.mp4
### 	m4 $< >$@
### 
### fit.mp dist.mp: circemacs.mp4
### 
### fit.11: fit.mp
### 	TEX=latex mpost $<
### 
### dist.1: dist.mp \
###   de-sband.dat dg-sband.dat de-tesla.dat dg-tesla.dat \
###   de-xband.dat dg-xband.dat de-sbandt.dat dg-sbandt.dat \
###   de-teslat.dat dg-teslat.dat de-xbandt.dat dg-xbandt.dat \
###   de-tesla3.dat dg-tesla3.dat de-tesla8.dat dg-tesla8.dat \
###   de-htesla.dat dg-htesla.dat de-htesla8.dat dg-htesla8.dat \
###   de-sband-ee.dat dg-sband-ee.dat de-tesla-ee.dat dg-tesla-ee.dat \
###   de-xband-ee.dat dg-xband-ee.dat de-sbandt-ee.dat dg-sbandt-ee.dat \
###   de-teslat-ee.dat dg-teslat-ee.dat de-xbandt-ee.dat dg-xbandt-ee.dat
### 	TEX=latex mpost $<
### 
### figures.1: figures.mp
### 	TEX=latex mpost $<
### 
### CPLOT = ./cplot | grep -v circe: | sed 's/E/e/g' | $(CPIF) $@
### 
### de-sband.dat: cplot Makefile
### 	echo 1 0.001 1.0 50 -1.0 11 11 500.0 1 $(V) $(REV) | $(CPLOT)
### 
### dg-sband.dat: cplot Makefile
### 	echo 1 0.001 1.0 50 -1.0 22 11 500.0 1 $(V) $(REV) | $(CPLOT)
### 
### de-tesla.dat: cplot Makefile
### 	echo 1 0.001 1.0 50 -1.0 11 11 500.0 2 $(V) $(REV) | $(CPLOT)
### 
### dg-tesla.dat: cplot Makefile
### 	echo 1 0.001 1.0 50 -1.0 22 11 500.0 2 $(V) $(REV) | $(CPLOT)
### 
### de-xband.dat: cplot Makefile
### 	echo 1 0.001 1.0 50 -1.0 11 11 500.0 3 $(V) $(REV) | $(CPLOT)
### 
### dg-xband.dat: cplot Makefile
### 	echo 1 0.001 1.0 50 -1.0 22 11 500.0 3 $(V) $(REV) | $(CPLOT)
### 
### de-sbandt.dat: cplot Makefile
### 	echo 1 0.001 1.0 50 -1.0 11 11 1000.0 1 $(V) $(REV) | $(CPLOT)
### 
### dg-sbandt.dat: cplot Makefile
### 	echo 1 0.001 1.0 50 -1.0 22 11 1000.0 1 $(V) $(REV) | $(CPLOT)
### 
### de-teslat.dat: cplot Makefile
### 	echo 1 0.001 1.0 50 -1.0 11 11 1000.0 2 $(V) $(REV) | $(CPLOT)
### 
### dg-teslat.dat: cplot Makefile
### 	echo 1 0.001 1.0 50 -1.0 22 11 1000.0 2 $(V) $(REV) | $(CPLOT)
### 
### de-xbandt.dat: cplot Makefile
### 	echo 1 0.001 1.0 50 -1.0 11 11 1000.0 3 $(V) $(REV) | $(CPLOT)
### 
### dg-xbandt.dat: cplot Makefile
### 	echo 1 0.001 1.0 50 -1.0 22 11 1000.0 3 $(V) $(REV) | $(CPLOT)
### 
### de-tesla3.dat: cplot Makefile
### 	echo 1 0.001 1.0 50 -1.0 11 11 350.0 2 $(V) $(REV) | $(CPLOT)
### 
### dg-tesla3.dat: cplot Makefile
### 	echo 1 0.001 1.0 50 -1.0 22 11 350.0 2 $(V) $(REV) | $(CPLOT)
### 
### de-tesla8.dat: cplot Makefile
### 	echo 1 0.001 1.0 50 -1.0 11 11 800.0 2 $(V) $(REV) | $(CPLOT)
### 
### dg-tesla8.dat: cplot Makefile
### 	echo 1 0.001 1.0 50 -1.0 22 11 800.0 2 $(V) $(REV) | $(CPLOT)
### 
### de-htesla.dat: cplot Makefile
### 	echo 1 0.001 1.0 50 -1.0 11 11 500.0 2 5 19980505 | $(CPLOT)
### 
### dg-htesla.dat: cplot Makefile
### 	echo 1 0.001 1.0 50 -1.0 22 11 500.0 2 5 19980505 | $(CPLOT)
### 
### de-htesla8.dat: cplot Makefile
### 	echo 1 0.001 1.0 50 -1.0 11 11 800.0 2 5 19980505 | $(CPLOT)
### 
### dg-htesla8.dat: cplot Makefile
### 	echo 1 0.001 1.0 50 -1.0 22 11 800.0 2 5 19980505 | $(CPLOT)
### 
### de-sband-ee.dat: cplot Makefile
### 	echo 1 0.001 1.0 50 -1.0 11 11 500.0 4 $(V) $(REV) | $(CPLOT)
### 
### dg-sband-ee.dat: cplot Makefile
### 	echo 1 0.001 1.0 50 -1.0 22 11 500.0 4 $(V) $(REV) | $(CPLOT)
### 
### de-tesla-ee.dat: cplot Makefile
### 	echo 1 0.001 1.0 50 -1.0 11 11 500.0 5 $(V) $(REV) | $(CPLOT)
### 
### dg-tesla-ee.dat: cplot Makefile
### 	echo 1 0.001 1.0 50 -1.0 22 11 500.0 5 $(V) $(REV) | $(CPLOT)
### 
### de-xband-ee.dat: cplot Makefile
### 	echo 1 0.001 1.0 50 -1.0 11 11 500.0 6 $(V) $(REV) | $(CPLOT)
### 
### dg-xband-ee.dat: cplot Makefile
### 	echo 1 0.001 1.0 50 -1.0 22 11 500.0 6 $(V) $(REV) | $(CPLOT)
### 
### de-sbandt-ee.dat: cplot Makefile
### 	echo 1 0.001 1.0 50 -1.0 11 11 1000.0 4 $(V) $(REV) | $(CPLOT)
### 
### dg-sbandt-ee.dat: cplot Makefile
### 	echo 1 0.001 1.0 50 -1.0 22 11 1000.0 4 $(V) $(REV) | $(CPLOT)
### 
### de-teslat-ee.dat: cplot Makefile
### 	echo 1 0.001 1.0 50 -1.0 11 11 1000.0 5 $(V) $(REV) | $(CPLOT)
### 
### dg-teslat-ee.dat: cplot Makefile
### 	echo 1 0.001 1.0 50 -1.0 22 11 1000.0 5 $(V) $(REV) | $(CPLOT)
### 
### de-xbandt-ee.dat: cplot Makefile
### 	echo 1 0.001 1.0 50 -1.0 11 11 1000.0 6 $(V) $(REV) | $(CPLOT)
### 
### dg-xbandt-ee.dat: cplot Makefile
### 	echo 1 0.001 1.0 50 -1.0 22 11 1000.0 6 $(V) $(REV) | $(CPLOT)
### 
### ########################################################################
### 
### %.pv: %.dvj
### 	xd -w -pa4rs $< -nopostscript
### 
### %.hpv: %.html
### 	arena file:`pwd`/$<
### 
### %.dvi: %.tex
### 	-latex $<
### 	makeindex -o $*.ind $*.idx
### 	# bibtex $*
### 	latex $<
### 	if grep -s 'Rerun to get cross-references right.' $*.log; then \
### 	  latex $<; \
### 	fi
### 
### %.dvj: %.dvi
### 	dvidvi -m '2:0,1($(HALFWIDTH),0)' $< $@
### #	dvitodvi '2:0+1($(HALFWIDTH),0)' $< > $@
### 	@echo
### 
### %.ps: %.dvj
### 	dvips -t landscape -t a4 -o $@ $<
### 
### %.html: %.nw
### 	$(WEAVE) -filter l2h -html -index -delay $< > $@
### 
### %.ps.gz: %.ps
### 	gzip -fv9 $<
### 
### clean:
### 	rm -f *~ $(SRCS) *.[cf] *.o \
### 		circe.html *.tex *.toc *.aux *.dv[ij] *.log *.rcs *.idx \
### 		*.ilg *.ind *.blg\
### 		lumidiff-* d[eg]-*.dat
### 
### realclean: clean
### 	rm -f sample*  *.bbl *.aux *.ind
### 
### snap: circe.f circe.h sample.f
### 	tar cf - Makefile $(WEBS) circe.bib  circe.f circe.h sample.f \
### 	  | gzip -9 > circe-current.tar.gz
### 
### TAR = circe.f circe.h sample.f Makefile $(WEBS) circe.bib prsty.bst \
### 	circemacs.mp4 fit.mp4 dist.mp4 graphx.mp \
### 	output/sband_fast/lumidiff-eex10.dat \
### 	output/sband_fast/lumidiff-eex10.fit \
### 	output/sband_fast/lumidiff-ggx10.dat \
### 	output/sband_fast/lumidiff-ggx10.fit \
### 	output/tesla_fast/lumidiff-eex10.dat \
### 	output/tesla_fast/lumidiff-eex10.fit \
### 	output/tesla_fast/lumidiff-ggx10.dat \
### 	output/tesla_fast/lumidiff-ggx10.fit \
### 	output/nlc_a_fast/lumidiff-eex10.dat \
### 	output/nlc_a_fast/lumidiff-eex10.fit \
### 	output/nlc_a_fast/lumidiff-ggx10.dat \
### 	output/nlc_a_fast/lumidiff-ggx10.fit \
### 	output/sband1000_fast/lumidiff-eex10.dat \
### 	output/sband1000_fast/lumidiff-eex10.fit \
### 	output/sband1000_fast/lumidiff-ggx10.dat \
### 	output/sband1000_fast/lumidiff-ggx10.fit \
### 	output/tesla1000_fast/lumidiff-eex10.dat \
### 	output/tesla1000_fast/lumidiff-eex10.fit \
### 	output/tesla1000_fast/lumidiff-ggx10.dat \
### 	output/tesla1000_fast/lumidiff-ggx10.fit \
### 	output/nlc1000_a_fast/lumidiff-eex10.dat \
### 	output/nlc1000_a_fast/lumidiff-eex10.fit \
### 	output/nlc1000_a_fast/lumidiff-ggx10.dat \
### 	output/nlc1000_a_fast/lumidiff-ggx10.fit \
### 	output/tesla350_fast/lumidiff-eex10.dat \
### 	output/tesla350_fast/lumidiff-eex10.fit \
### 	output/tesla350_fast/lumidiff-ggx10.dat \
### 	output/tesla350_fast/lumidiff-ggx10.fit \
### 	output/tesla800_fast/lumidiff-eex10.dat \
### 	output/tesla800_fast/lumidiff-eex10.fit \
### 	output/tesla800_fast/lumidiff-ggx10.dat \
### 	output/tesla800_fast/lumidiff-ggx10.fit \
### 	output/Slices.mp4 \
### 	fit.11 fit.12 fit.13 fit.14 fit.15 fit.16 fit.17 fit.18 \
### 	fit.21 fit.22 fit.23 fit.24 fit.25 fit.26 fit.27 fit.28 \
### 	dist.1 dist.2 dist.3 dist.4 dist.5 dist.6
### 
### tar: circe.tar.gz
### circe.tar.gz: $(TAR)
### 	tar cf - $(TAR) | gzip -9 >$@
### 
### test.tar: tar
### 	rm -fr tmp
### 	mkdir tmp
### 	zcat circe.tar.gz | (cd tmp; tar xf -)
### 	cd tmp; make pv
### 
### disk: circe.f circe.h sample.f circe.tar.gz manual.ps.gz circe.ps.gz
### 	tar cvvf /dev/fd0 circe.f circe.h sample.f \
### 		circe.tar.gz manual.ps.gz circe.ps.gz
### 
### hep-ph.tar.gz: manual.tex fit.mp dist.mp figures.mp
### 	rm -fr public
### 	mkdir public
### 	perl -ne '\
### 	  s/(\\nw(begin|end)docs\{\d*\})/$$1%\n/g; \
### 	  s/\\sublabel.*((\\moddef.*)(\\(plus)?endmoddef)).*/$$2%\n$$3/g; \
### 	  s/~\{\\nwtagstyle.*\}\}\}/\}/g; \
### 	  s/^.*(\\nwendcode\{\})/$$1%\n/g; \
### 	  s/\\subpageref\{[^}]+\}//g; \
###           print unless (/thopp/ || /preprintno/ || /\\nwixlogsorted/ );' \
### 	    manual.tex > public/manual.tex
### 	grep -v euler fit.mp > public/fit.mp
### 	grep -v euler dist.mp > public/dist.mp
### 	grep -v euler figures.mp > public/figures.mp
### 	cp circe.bib Makefile public
### 	cd public && ln -s ../graphx.mp ../prsty.bst .
### 	cd public && ln -s ../output ../d[eg]-*.dat .
### 	cd public && cp /usr/local/lib/texmf/tex/inputs/noweb.sty .
### 	cd public && TEX=latex mpost fit
### 	cd public && TEX=latex mpost dist
### 	cd public && TEX=latex mpost figures
### 	cd public && latex manual
### 	cd public && latex manual
### 	cd public && dvips manual
### 	tar cvvzf $@ public/manual.tex public/noweb.sty public/*.[0-9]*
### 
### hep-ph.test: hep-ph.tar.gz
### 	rm -fr public
### 	tar xvvzf $<
### 	cd public && latex manual
### 	cd public && latex manual
### 	cd public && dvips manual
### 
### CPC.tar.gz: manual.ps circe.f circe.h sample.f
### 	tar cvvzf $@ $^
### 
### ########################################################################
### 
### fit_v1.f: minuit.nw $(BRACKET)
### 	cat $(TRIPLE) | $(FTANGLE) -R'[[$@]]' | $(CPIF) $@
### 
### fit_v1.bin: fit_v1.f
### 	$(FC) -fno-automatic -O2 -o $@ $< \
### 	  $(CERNLIBDIR) -lpacklib -lmathlib -lkernlib # -lieee
### 
### fit_v1: minuit.nw $(BRACKET)
### 	cat $(TRIPLE) | $(FTANGLE) -R'[[$@]]' | $(CPIF) $@
### 	chmod 755 $@
### 
### minuit1.f: minuit.nw $(BRACKET)
### 	cat $(TRIPLE) | $(FTANGLE) -R'[[$@]]' | $(CPIF) $@
### 
### minuit2.f: minuit.nw $(BRACKET)
### 	cat $(TRIPLE) | $(FTANGLE) -R'[[$@]]' | $(CPIF) $@
### 
### minuit1.bin: minuit1.f
### 	$(FC) -O2 -o $@ $< $(CERNLIBDIR) -lpacklib -lieee
### 
### minuit2.bin: minuit2.f
### 	$(FC) -O2 -o $@ $< $(CERNLIBDIR) -lpacklib -lieee
### 
### minuit1.sh: minuit.nw $(BRACKET)
### 	cat $(TRIPLE) | $(FTANGLE) -R'[[$@]]' | $(CPIF) $@
### 
### minuit2.sh: minuit.nw $(BRACKET)
### 	cat $(TRIPLE) | $(FTANGLE) -R'[[$@]]' | $(CPIF) $@
### 
### ########################################################################
