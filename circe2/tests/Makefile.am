# Makefile.am --
# $Id: Makefile.am 314 2010-04-17 20:32:33Z ohl $
########################################################################

TESTS = \
  circe2_tool.run \
  fail.run \
  flat.run moderate.run steep.run low.run \
  power.run resonance.run \
  multi.run delta.run \
  smooth.run oversmooth.run \
  scale.run

XFAIL_TESTS =

BUILT_SOURCES = \
    OCAML_FLAG

OCAML_FLAG:
if OCAML_AVAILABLE
	touch $@
endif

EXTRA_DIST = $(TESTS:.run=.sh)

VPATH = $(srcdir)
SUFFIXES = .sh .run

.sh.run:
	@rm -f $@
	$(SED) 's|@name@|$*|g' $< >$@
	chmod +x $@

## Remove generated files
clean-local:
	rm -f *.output *.run *.circe2 OCAML_FLAG

maintainer-clean-local:
	-rm -f *~
.PHONY: maintainer-clean-local

if OCAML_AVAILABLE
installcheck-local:
	PATH=$(DESTDIR)$(bindir):$$PATH; export PATH; \
	LD_LIBRARY_PATH=$(DESTDIR)$(libdir):$(DESTDIR)$(pkglibdir):$$LD_LIBRARY_PATH; \
	export LD_LIBRARY_PATH; \
	circe2_tool.opt -test || circe2_tool.bin -test || circe2_tool -test
endif
