if PYTHIA_AVAILABLE

if FC_IS_NAG
AM_FFLAGS = -dcfuns -w
endif
bin_PROGRAMS = interface

if STDHEP_AVAILABLE
interface_SOURCES = main_stdhep.f interface.f pythia.f
interface_LDADD = $(LDFLAGS_STDHEP)
else
interface_SOURCES = main_lhef.f interface.f pythia.f
endif	

endif	
