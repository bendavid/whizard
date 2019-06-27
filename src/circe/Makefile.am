# Makefile for compiling the CIRCE library

## Redefining variables in this way is not idiomatic automake.
LIB=${libdir}

TARGET=circe.o
OBJECTS=$(TARGET)

# Targets
#all: obj

#obj: $(OBJECTS)

#install: $(TARGET)
	cp $(TARGET) $(LIB)/$(TARGET)

## OpenMP
AM_FCFLAGS =
AM_LDFLAGS =
if WHIZARD_WITH_OPENMP
AM_FCFLAGS += $(OPENMP_FCFLAGS)
AM_LDFLAGS += $(OPENMP_FCFLAGS)
endif

# Cleanup
maintainer-clean-local:
	-rm -f *~ $(TARGET)

#realclean: clean

#distclean: realclean
#	rm -f Makefile

