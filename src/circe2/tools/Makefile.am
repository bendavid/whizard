# Makefile.am --
# $Id: Makefile.am 314 2010-04-17 20:32:33Z ohl $
########################################################################
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

bin_PROGRAMS = circe2_sample circe2_sample_binary circe2ls \
   sample_taorng

circe2_sample_SOURCES = circe2_sample.f90
circe2_sample_LDADD = $(top_builddir)/src/libcirce2.la
circe2_sample_binary_SOURCES = circe2_sample_binary.f90 write3_binary.c
circe2_sample_binary_LDADD = $(top_builddir)/src/libcirce2.la
sample_taorng_SOURCES = sample_taorng.f90
sample_taorng_LDADD = $(top_builddir)/src/libcirce2.la

circe2ls_SOURCES = circe2ls.f90
circe2ls_LDADD = $(top_builddir)/src/libcirce2.la

# Fortran90 module files are generated at the same time as object files
.lo.$(FC_MODULE_EXT):
	@:

AM_FFLAGS = 
AM_FCFLAGS = -I$(top_builddir)/src

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

WEBS = $(top_srcdir)/src/prelude.nw $(top_srcdir)/src/circe2.nw \
  $(top_srcdir)/src/postlude.nw

SUFFIXES = .nw .$(FC_MODULE_EXT)

.nw.f90:
	$(NOTANGLE_IT)

if NOWEB_AVAILABLE
circe2_sample.f90: $(WEBS)
	cat $(WEBS) | $(NOTANGLE) -R'[[$@]]' | $(CPIF) $@

circe2_sample_binary.f90: $(WEBS)
	cat $(WEBS) | $(NOTANGLE) -R'[[$@]]' | $(CPIF) $@

circe2ls.f90: $(WEBS)
	cat $(WEBS) | $(NOTANGLE) -R'[[$@]]' | $(CPIF) $@

sample_taorng.f90: $(WEBS)
	cat $(WEBS) | $(NOTANGLE) -R'[[$@]]' | $(CPIF) $@

write3_binary.c: $(WEBS)
	cat $(WEBS) | $(NOTANGLE) -R'[[$@]]' | $(CPIF) $@
endif NOWEB_AVAILABLE

########################################################################
### Old demos and data construction routines
########################################################################
# OPTIONS = triangle
# OPTIONS = triangle bins = 20  map = id { 50 [0.75,0.85] } map = id { 5 [0.85,1] }
OPTIONS = bins = 10  map = id { 40 [0.75,1] }

EVENTS = foo
NEVENTS = 1000000

DEMOS = demo1 demo2 demo3 demo4 demo5 demo6 demo7 demo8 demo9

noinst_PROGRAMS = $(DEMOS)
demo1_SOURCES = demo1.f90
demo2_SOURCES = demo2.f90
demo3_SOURCES = demo3.f90
demo4_SOURCES = demo4.f90
demo5_SOURCES = demo5.f90
demo6_SOURCES = demo6.f90
demo7_SOURCES = demo7.f90
demo8_SOURCES = demo8.f90
demo9_SOURCES = demo9.f90
demo1_LDADD = $(top_builddir)/src/libcirce2.la
demo2_LDADD = $(top_builddir)/src/libcirce2.la
demo3_LDADD = $(top_builddir)/src/libcirce2.la
demo4_LDADD = $(top_builddir)/src/libcirce2.la
demo5_LDADD = $(top_builddir)/src/libcirce2.la
demo6_LDADD = $(top_builddir)/src/libcirce2.la
demo7_LDADD = $(top_builddir)/src/libcirce2.la
demo8_LDADD = $(top_builddir)/src/libcirce2.la
demo9_LDADD = $(top_builddir)/src/libcirce2.la


demo1.f90: $(top_srcdir)/src/prelude.nw
	 sed -n -e '/^  program demo1$$/,/^  end program demo1$$/p' \
		-e '/^  subroutine random/,/^  end subroutine random$$/p' $< >$@
demo2.f90: $(top_srcdir)/src/prelude.nw
	 sed -n -e '/^  program demo2$$/,/^  end program demo2$$/p' \
		-e '/^  subroutine random/,/^  end subroutine random$$/p' $< >$@
demo3.f90: $(top_srcdir)/src/prelude.nw
	 sed -n -e '/^  program demo3$$/,/^  end program demo3$$/p' \
		-e '/^  subroutine random/,/^  end subroutine random$$/p' $< >$@
demo4.f90: $(top_srcdir)/src/prelude.nw
	 sed -n -e '/^  program demo4$$/,/^  end program demo4$$/p' \
		-e '/^  subroutine random/,/^  end subroutine random$$/p' $< >$@
demo5.f90: $(top_srcdir)/src/prelude.nw
	 sed -n -e '/^  program demo5$$/,/^  end program demo5$$/p' \
		-e '/^  subroutine random/,/^  end subroutine random$$/p' $< >$@
demo6.f90: $(top_srcdir)/src/prelude.nw
	 sed -n -e '/^  program demo6$$/,/^  end program demo6$$/p' \
		-e '/^  subroutine random/,/^  end subroutine random$$/p' $< >$@
demo7.f90: $(top_srcdir)/src/prelude.nw
	 sed -n -e '/^  program demo7$$/,/^  end program demo7$$/p' \
		-e '/^  subroutine random/,/^  end subroutine random$$/p' $< >$@
demo8.f90: $(top_srcdir)/src/prelude.nw
	 sed -n -e '/^  program demo8$$/,/^  end program demo8$$/p' \
		-e '/^  subroutine random/,/^  end subroutine random$$/p' $< >$@
demo9.f90: $(top_srcdir)/src/prelude.nw
	 sed -n -e '/^  program demo9$$/,/^  end program demo9$$/p' \
		-e '/^  subroutine random/,/^  end subroutine random$$/p' $< >$@

test: 
	$(top_builddir)/src/circe2_tool.opt -c '\
          { file = "$(EVENTS).circe" \
            { design = "*" roots = 500 comment = "Circe2 trivial self test" \
              { pid = gamma events = "$(EVENTS).map" binary $(OPTIONS) } } }'
########################################################################
# Former construction IO = binary or IO = ascii not portable to automake
### IO = binary
	echo "'$(EVENTS).circe' '*' 500 22 0 22 0 'unweighted' $(NEVENTS)" \
		| ./circe2_sample_binary > $(EVENTS).new.map
	echo "'$(EVENTS).circe' '*' 500 22 0 22 0 'weighted' $(NEVENTS)" \
		| ./circe2_sample_binary > $(EVENTS).new-w.map
# ### IO = ascii
#	echo "'$(EVENTS).circe' '*' 500 22 0 22 0 'unweighted' $(NEVENTS)" \
#		| ./circe2_sample \
#		| $(top_builddir)/src/circe2_tool.opt -ob $(EVENTS).new.map
#	echo "'$(EVENTS).circe' '*' 500 22 0 22 0 'weighted' $(NEVENTS)" \
#		| ./circe2_sample \
#		| $(top_builddir)/src/circe2_tool.opt -ob $(EVENTS).new-w.map
	$(top_builddir)/src/circe2_tool.opt -hb $(EVENTS).new.map -p .new.histo
	$(top_builddir)/src/circe2_tool.opt -hb $(EVENTS).new-w.map -p .new-w.histo

xy.histo: $(EVENTS).map
	$(top_builddir)/src/circe2_tool.opt -hb $< -p .histo

.events.map: 
	$(top_builddir)/src/circe2_tool.opt -ia $< -ob $@

test-block-data:
	$(top_builddir)/src/circe2_tool.opt -c '\
          { file = "foo.circe" block_data = "foo.f" \
            { design = "*" roots = 500 \
              { pid = gamma events = "foo.map" binary bins = 50 iterations = 1 } } }'

test-parser1:
	$(top_builddir)/src/circe2_tool.opt -c '\
          { file = "foo.circe" \
            { design = "*" roots = 500 \
              { pid = gamma events = "foo.1" binary \
                bins/1 = 1 bins/2 = 20 \
                map/1 = id { 1 [0.2,0.4] } \
                map/1 = id { 1 [0.5,0.6] } } } }'

test-parser2:
	$(top_builddir)/src/circe2_tool.opt -c '\
          { file = "foo.circe" \
            { design = "*" roots = 500 \
              { pid = gamma events = "foo.1" binary \
                bins/1 = 10 \
                map/1 = id { 10 [0.2,0.4] } \
                map/1 = power { 10 [0.5,0.6] beta = -0.3 eta = 0 } \
                bins/2 = 20 } } }'

test-parser3:
	$(top_builddir)/src/circe2_tool.opt -c '\
          { file = "foo.circe" \
            { design = "*" roots = 500 \
              { pid = gamma events = "foo.1" binary \
                bins/1 = 10 \
                map/1 = id { 10 [0.2,0.4] } \
                map/1 = power { 10 [0.5,0.6] beta = -1.3 eta = 0 } \
                bins/2 = 20 } } }'

test-parser4:
	$(top_builddir)/src/circe2_tool.opt -c '\
          { file = "foo.circe" \
            { design = "*" roots = 500 \
              { pid = gamma events = "foo.1" binary \
                bins/1 = 1 \
                map/1 = id { 1 [0.2,0.4] } \
                map/1 = resonance { 1 [0.5,0.6] center = 0 width = 1 } \
                bins/2 = 20 } } }'

big-test:
	$(top_builddir)/src/circe2_tool.opt -c '\
          { file = "foo.circe" \
            { design = "test/bins=10" roots = 500 \
              { pid = gamma events = "foo.map" binary bins = 10 $(OPTIONS) } } \
            { design = "test/bins=20" roots = 500 \
              { pid = gamma events = "foo.map" binary bins = 20 $(OPTIONS) } } \
            { design = "test/bins=50" roots = 500 \
              { pid = gamma events = "foo.map" binary bins = 50 $(OPTIONS) } } \
            { design = "test/bins=100" roots = 500 \
              { pid = gamma events = "foo.map" binary bins = 100 $(OPTIONS) } } \
            { design = "test/bins=10/t" roots = 500 \
              { pid = gamma events = "foo.map" binary bins = 10 triangle $(OPTIONS) } } \
            { design = "test/bins=20/t" roots = 500 \
              { pid = gamma events = "foo.map" binary bins = 20 triangle $(OPTIONS) } } \
            { design = "test/bins=50/t" roots = 500 \
              { pid = gamma events = "foo.map" binary bins = 50 triangle $(OPTIONS) } } \
            { design = "test/bins=100/t" roots = 500 \
              { pid = gamma events = "foo.map" binary bins = 100 triangle $(OPTIONS) } } }'


########################################################################
# Old targets to produce the 20/50 bin data files. Depending on external
# data files from D. Schulte and V. Telnov as well as some O'Caml code
# not included here.
########################################################################

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
### 	$(top_builddir)/src/circe2_tool.opt -f teslagg_200.commands
### 	$(top_builddir)/src/circe2_tool.opt -f teslagg_500.commands
### 	$(top_builddir)/src/circe2_tool.opt -f teslagg_800.commands
### 	$(top_builddir)/src/circe2_tool.opt -f teslage_500.commands
### 
### valery-data-circe2-polavg:
### 	$(top_builddir)/src/circe2_tool.opt -f teslagg_200_polavg.commands
### 	$(top_builddir)/src/circe2_tool.opt -f teslagg_500_polavg.commands
### 	$(top_builddir)/src/circe2_tool.opt -f teslagg_800_polavg.commands
### 	$(top_builddir)/src/circe2_tool.opt -f teslage_500_polavg.commands

########################################################################
# The following line just says
#    include Makefile.depend
# but in a portable fashion (depending on automake's AM_MAKE_INCLUDE
########################################################################
@am__include@ @am__quote@Makefile.depend@am__quote@

Makefile.depend: $(circe2_sample_SOURCES) $(circe2_sample_binary_SOURCES) \
		$(circe2ls_SOURCES) $(sample_taorng_SOURCES)
	@rm -f $@
	for src in $^; do \
          module="`basename $$src | sed 's/\.f90//'`"; \
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

########################################################################

########################################################################
## Non-standard cleanup tasks
## Remove sources that can be recreated using NOWEB
if NOWEB_AVAILABLE
maintainer-clean-noweb:
	-rm -f *.f90 *.c 
endif
.PHONY: maintainer-clean-noweb

## Remove those sources also if builddir and srcdir are different
if NOWEB_AVAILABLE
clean-noweb:
	test "$(srcdir)" != "." && rm -f *.f90 *.c || true
endif
.PHONY: clean-noweb

## Remove F90 module files
clean-local: clean-noweb
	-rm -f *.$(FC_MODULE_EXT) *.g90 

## Remove backup files
maintainer-clean-backup:
	-rm -f *~
.PHONY: maintainer-clean-backup

## Register additional clean targets
maintainer-clean-local: maintainer-clean-noweb maintainer-clean-backup

###           module="`basename $$src | sed 's/\.f[90][0358]//'`"; 
########################################################################
# MPI
########################################################################
###
### # The -mismatch_all is for mpi_send() etc.
### MPIFC = mpif90
### MPIFCFLAGS = # -mismatch_all
