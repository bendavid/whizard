# Makefile.am -- Makefile for O'Mega within and without WHIZARD
# $Id: Makefile.am 7444 2016-02-17 15:37:20Z jr_reuter $
##
## Process this file with automake to produce Makefile.in
##
########################################################################
#
# Copyright (C) 1999-2016 by 
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

### N = 100
### TOLERANCE = 1000000
### 
### auxdir = $(top_srcdir)/src/misc
### 
### build_bindir = $(top_srcdir)/bin
### build_libdir = $(top_srcdir)/lib
### build_srcdir = $(top_srcdir)/tests/MSSM
### build_tooldir = $(top_srcdir)/tools
### 
### OMEGA_QED = $(build_bindir)/f90_QED.opt
### OMEGA_SM = $(build_bindir)/f90_MSSM.opt
### 
### OFLAGS = -old-interface \
### 	-target:function $(@:_module.f90=) \
### 	-target:module $(@:.f90=) \
### 	-target:parameter_module omega_parameters_mssm_4
### 
### FC_OPT = @FC_OPT@
### FC_FLAGS = $(FC_OPT) -I$(build_libdir)
### 
### if FC_IMPURE
### FC_FILTER = \
### 	sed -e '/^[ 	]*elemental[	 ]/s/elemental[	 ]//' \
### 	    -e '/^[ 	]*pure[	 ]/s/pure[	 ]//' | $(CPIF)
### else
### FC_FILTER = $(CPIF)
### endif
### 
### HELAS = dhelas95
### 
### MADGRAPH = @MADGRAPH@
### MG_QED = echo 0; echo; echo; echo
### MG_SM = echo 0; echo yes; echo; echo
### 
### RUN_MADGRAPH = $(top_srcdir)/$(MADGRAPH); rm $(@:.f90=.ps); mv $(@:.f90=.f) $@   
### 
### LIBS = $(build_libdir)/libomega95.a $(build_libdir)/libomega95_tools.a
### 
### FC_LIB_FLAGS = -L$(build_libdir) -lomega95_tools -lomega95 -L. -l$(HELAS)
### 
### OMEGA_SRC4 = \
###   odbd_wpwm_module.f90 \
###   obbb_wpwm_module.f90 ozz_hh_module.f90 \
###   oepem_wpwm_module.f90 owpwm_wpwm_module.f90 \
###   owpwm_zz_module.f90 owpwm_za_module.f90 owpwm_aa_module.f90 \
###   oepem_epem_module.f90 oepem_veve_module.f90 \
###   oudb_udb_module.f90 oepem_mumu_module.f90 \
###   oemem_emem_module.f90 oema_ema_module.f90 \
###   oaa_epem_module.f90 oza_epem_module.f90 \
###   oza_uub_module.f90 oza_ddb_module.f90 \
###   ozz_epem_module.f90 ozz_veve_module.f90 \
###   oepem_aa_module.f90 oepem_za_module.f90 oepem_zz_module.f90
### 
### OMEGA_SRC5 = \
###   oepem_epema_module.f90 oemem_emema_module.f90 \
###   oepem_aaa_module.f90 oepem_zaa_module.f90 \
###   oemep_emvewp_module.f90 \
###   oepem_wpwmz_module.f90 oepem_wpwma_module.f90
### 
### OMEGA_SRC6 = \
###   oepem_muvmtavt_module.f90 oepem_epveemve_module.f90 \
###   oepem_mumuaa_module.f90 oepem_epemaa_module.f90 \
###   omuem_muemaa_module.f90 oemem_ememaa_module.f90 \
###   oepem_aaaa_module.f90 oepem_epemepem_module.f90 \
###   oemep_emvewpa_module.f90 oemep_vevewpwm_module.f90 \
###   oemep_emepwpwm_module.f90 owpwm_uubssb_module.f90 \
###   oepem_vevebbb_module.f90
### 
### OMEGA_SRC7 = \
###   oepem_muvmtavta_module.f90 oemep_emveudba_module.f90 \
###   oepem_aaaaa_module.f90 oepem_epemaaa_module.f90 \
###   oepem_epemepema_module.f90 oaa_epemaaa_module.f90 \
###   oaa_epemmumua_module.f90 oaa_epemepema_module.f90 \
###   oepem_veveuubz_module.f90
### 
### OMEGA_SRC8 = \
###   oepem_muvmtavtaa_module.f90 oepem_epemaaaa_module.f90 \
###   oepem_mumutatauub_module.f90 oepem_muvmtavtuub_module.f90 \
###   oepem_vevemuvmudb_module.f90
### 
### OMEGA_SRCX = \
###   oepem_wpwmaa_module.f90 \
###   oepem_muvmtavtaa_module.f90 \
###   owpwm_zaa_module.f90 owpwm_aaa_module.f90 owpwm_wpwma_module.f90 \
###   oepem_epvebbbdub_module.f90
### 
### # OMEGA_SRCT = \
### #   single_top_module.f90 \
### #   single_top_fudged_module.f90 \
### #   single_top_constant_module.f90
### 
### OMEGA_SRCT = oepem_wpwm_module.f90
### 
### OMEGA_SRC = \
### 	$(OMEGA_SRC4) $(OMEGA_SRC5) $(OMEGA_SRC6) \
### 	$(OMEGA_SRC7) $(OMEGA_SRC8) $(OMEGA_SRCX)
### 
### MADGRAPH_SRC4 = $(patsubst o%_module.f90,%.f90, $(OMEGA_SRC4))
### MADGRAPH_SRC5 = $(patsubst o%_module.f90,%.f90, $(OMEGA_SRC5))
### MADGRAPH_SRC6 = $(patsubst o%_module.f90,%.f90, $(OMEGA_SRC6))
### MADGRAPH_SRC7 = $(patsubst o%_module.f90,%.f90, $(OMEGA_SRC7))
### MADGRAPH_SRC8 = $(patsubst o%_module.f90,%.f90, $(OMEGA_SRC8))
### MADGRAPH_SRCX = $(patsubst o%_module.f90,%.f90, $(OMEGA_SRCX))
### MADGRAPH_SRC = $(patsubst o%_module.f90,%.f90, $(OMEGA_SRC))
### 
### OMEGA_OBJ4 = $(OMEGA_SRC4:.f90=.o)
### OMEGA_OBJ5 = $(OMEGA_SRC5:.f90=.o)
### OMEGA_OBJ6 = $(OMEGA_SRC6:.f90=.o)
### OMEGA_OBJ7 = $(OMEGA_SRC7:.f90=.o)
### OMEGA_OBJ8 = $(OMEGA_SRC8:.f90=.o)
### OMEGA_OBJX = $(OMEGA_SRCX:.f90=.o)
### OMEGA_OBJ = $(OMEGA_SRC:.f90=.o)
### 
### OMEGA_OBJT = $(OMEGA_SRCT:.f90=.o)
### 
### all: main4 main5 main6 main7 main8 mainx
### 
### runall: run4 run5 run6 run7 run8 runx
### 
### run%: main%
### 	echo N = $(N), TOLERANCE = $(TOLERANCE) | ./$<
### 
### ########################################################################
### 
### OBJS4 = madgraph4.o $(OMEGA_OBJ4) omega_amplitudes4.o
### OBJS5 = madgraph5.o $(OMEGA_OBJ5) omega_amplitudes5.o
### OBJS6 = madgraph6.o $(OMEGA_OBJ6) omega_amplitudes6.o
### OBJS7 = madgraph7.o $(OMEGA_OBJ7) omega_amplitudes7.o
### OBJS8 = madgraph8.o $(OMEGA_OBJ8) omega_amplitudes8.o
### OBJSX = madgraphx.o $(OMEGA_OBJX) omega_amplitudesx.o
### OBJST = $(OMEGA_OBJT) omega_amplitudest.o
### 
### ########################################################################
### # There are no Modula(n) sources here ...
### %.o: %.mod
### ########################################################################
### 
### $(build_srcdir)/%.$(FC_SRC_EXT): %.f90
### 	cat $< | $(FC_FILTER) $(build_srcdir)/$*.$(FC_SRC_EXT)
### 
### %.o: $(build_srcdir)/%.$(FC_SRC_EXT) parameters.MSSM_4.omega.o
### 	$(FC) $(FC_FLAGS) -c -o $@ $<
### 
### %_p.o: $(build_srcdir)/%.$(FC_SRC_EXT) parameters.MSSM_4.omega.o
### 	$(FC) $(FC_FLAGS) -c -o $@ $<
### 
### ########################################################################
### 
### main4: main4.o $(LIBS) 
### 	$(FC) $(FC_FLAGS) -o $@ parameters.MSSM_4.omega.o $(OBJS4) \
### 	 main4.o $(FC_LIB_FLAGS)
### 
### main5: main5.o $(LIBS)
### 	$(FC) $(FC_FLAGS) -o $@ $(OBJS5) main5.o $(FC_LIB_FLAGS)
### 
### main6: main6.o $(LIBS)
### 	$(FC) $(FC_FLAGS) -o $@ $(OBJS6) main6.o $(FC_LIB_FLAGS)
### 
### main7: main7.o $(LIBS)
### 	$(FC) $(FC_FLAGS) -o $@ $(OBJS7) main7.o $(FC_LIB_FLAGS)
### 
### main8: main8.o $(LIBS)
### 	$(FC) $(FC_FLAGS) -o $@ $(OBJS8) main8.o $(FC_LIB_FLAGS)
### 
### mainx: mainx.o $(LIBS)
### 	$(FC) $(FC_FLAGS) -o $@ $(OBJSX) mainx.o $(FC_LIB_FLAGS)
### 
### maint: maint.o $(LIBS)
### 	$(FC) $(FC_FLAGS) -o $@ $(OBJST) maint.o $(FC_LIB_FLAGS)
### 
### madgraph4.o: $(build_srcdir)/madgraph4.$(FC_SRC_EXT) lib$(HELAS).a
### 	$(FC) $(FC_FLAGS) -c -o $@ $<
### 
### madgraph5.o: $(build_srcdir)/madgraph5.$(FC_SRC_EXT) lib$(HELAS).a
### 	$(FC) $(FC_FLAGS) -c -o $@ $<
### 
### madgraph6.o: $(build_srcdir)/madgraph6.$(FC_SRC_EXT) lib$(HELAS).a
### 	$(FC) $(FC_FLAGS) -c -o $@ $<
### 
### madgraph7.o: $(build_srcdir)/madgraph7.$(FC_SRC_EXT) lib$(HELAS).a
### 	$(FC) $(FC_FLAGS) -c -o $@ $<
### 
### madgraph8.o: $(build_srcdir)/madgraph8.$(FC_SRC_EXT) lib$(HELAS).a
### 	$(FC) $(FC_FLAGS) -c -o $@ $<
### 
### madgraphx.o: $(build_srcdir)/madgraphx.$(FC_SRC_EXT) lib$(HELAS).a
### 	$(FC) $(FC_FLAGS) -c -o $@ $<
### 
### ########################################################################
### #
### # 4 external lines:
### #
### ########################################################################
### 
### ozz_hh_module.f90: $(OMEGA_SM)
### 	$(OMEGA_SM) $(OFLAGS) -scatter "Z Z -> H H" >$@
### 
### odbd_wpwm_module.f90: $(OMEGA_SM)
### 	$(OMEGA_SM) $(OFLAGS) -scatter "dbar d -> W+ W-" >$@
### 
### obbb_wpwm_module.f90: $(OMEGA_SM)
### 	$(OMEGA_SM) $(OFLAGS) -scatter "bbar b -> W+ W-" >$@
### 
### owpwm_aa_module.f90: $(OMEGA_SM)
### 	$(OMEGA_SM) $(OFLAGS) -scatter "W+ W- -> A A" >$@
### 
### owpwm_za_module.f90: $(OMEGA_SM)
### 	$(OMEGA_SM) $(OFLAGS) -scatter "W+ W- -> Z A" >$@
### 
### owpwm_zz_module.f90: $(OMEGA_SM)
### 	$(OMEGA_SM) $(OFLAGS) -scatter "W+ W- -> Z Z" >$@
### 
### owpwm_wpwm_module.f90: $(OMEGA_SM)
### 	$(OMEGA_SM) $(OFLAGS) -scatter "W+ W- -> W+ W-" >$@
### 
### oepem_wpwm_module.f90: $(OMEGA_SM)
### 	$(OMEGA_SM) $(OFLAGS) -scatter "e+ e- -> W+ W-" >$@
### 
### oepem_epem_module.f90: $(OMEGA_SM)
### 	$(OMEGA_SM) $(OFLAGS) -scatter "e+ e- -> e+ e-" >$@
### 
### oepem_veve_module.f90: $(OMEGA_SM)
### 	$(OMEGA_SM) $(OFLAGS) -scatter "e+ e- -> nue nuebar" >$@
### 
### oudb_udb_module.f90: $(OMEGA_SM)
### 	$(OMEGA_SM) $(OFLAGS) -scatter "u dbar -> u dbar" >$@
### 
### oepem_mumu_module.f90: $(OMEGA_QED)
### 	$(OMEGA_QED) $(OFLAGS) -scatter "e+ e- -> m+ m-" >$@
### 
### oepem_aa_module.f90: $(OMEGA_QED)
### 	$(OMEGA_QED) $(OFLAGS) -scatter "e+ e- -> A A" >$@
### 
### oepem_za_module.f90: $(OMEGA_SM)
### 	$(OMEGA_SM) $(OFLAGS) -scatter "e+ e- -> Z A" >$@
### 
### oepem_zz_module.f90: $(OMEGA_SM)
### 	$(OMEGA_SM) $(OFLAGS) -scatter "e+ e- -> Z Z" >$@
### 
### oaa_epem_module.f90: $(OMEGA_QED)
### 	$(OMEGA_QED) $(OFLAGS) -scatter "A A -> e+ e-" >$@
### 
### oza_epem_module.f90: $(OMEGA_SM)
### 	$(OMEGA_SM) $(OFLAGS) -scatter "Z A -> e+ e-" >$@
### 
### ozz_epem_module.f90: $(OMEGA_SM)
### 	$(OMEGA_SM) $(OFLAGS) -scatter "Z Z -> e+ e-" >$@
### 
### ozz_veve_module.f90: $(OMEGA_SM)
### 	$(OMEGA_SM) $(OFLAGS) -scatter "Z Z -> nue nuebar" >$@
### 
### oza_uub_module.f90: $(OMEGA_SM)
### 	$(OMEGA_SM) $(OFLAGS) -scatter "Z A -> u ubar" >$@
### 
### oza_ddb_module.f90: $(OMEGA_SM)
### 	$(OMEGA_SM) $(OFLAGS) -scatter "Z A -> d dbar" >$@
### 
### oemem_emem_module.f90: $(OMEGA_QED)
### 	$(OMEGA_QED) $(OFLAGS) -scatter "e- e- -> e- e-" >$@
### 
### oema_ema_module.f90: $(OMEGA_QED)
### 	$(OMEGA_QED) $(OFLAGS) -scatter "e- A -> e- A" >$@
### 
### ifneq ($(MADGRAPH),false)
### 
### zz_hh.f90:
### 	(echo "z z -> h h"; $(MG_SM)) | $(RUN_MADGRAPH)
### 
### dbd_wpwm.f90:
### 	(echo "d~ d -> w+ w-"; $(MG_SM)) | $(RUN_MADGRAPH)
### 
### bbb_wpwm.f90:
### 	(echo "b~ b -> w+ w-"; $(MG_SM)) | $(RUN_MADGRAPH)
### 
### wpwm_wpwm.f90:
### 	(echo "w+ w- -> w+ w-"; $(MG_SM)) | $(RUN_MADGRAPH)
### 
### wpwm_aa.f90:
### 	(echo "w+ w- -> a a"; $(MG_SM)) | $(RUN_MADGRAPH)
### 
### wpwm_za.f90:
### 	(echo "w+ w- -> z a"; $(MG_SM)) | $(RUN_MADGRAPH)
### 
### wpwm_zz.f90:
### 	(echo "w+ w- -> z z"; $(MG_SM)) | $(RUN_MADGRAPH)
### 
### epem_wpwm.f90:
### 	(echo "e+ e- -> w+ w-"; $(MG_SM)) | $(RUN_MADGRAPH)
### 
### epem_epem.f90:
### 	(echo "e+ e- -> e+ e-"; $(MG_SM)) | $(RUN_MADGRAPH)
### 
### udb_udb.f90:
### 	(echo "u d~ -> u d~"; $(MG_SM)) | $(RUN_MADGRAPH)
### 
### epem_veve.f90:
### 	(echo "e+ e- -> ve ve~"; $(MG_SM)) | $(RUN_MADGRAPH)
### 
### epem_mumu.f90:
### 	(echo "e+ e- -> mu+ mu-"; $(MG_QED)) | $(RUN_MADGRAPH)
### 
### epem_aa.f90:
### 	(echo "e+ e- -> a a"; $(MG_QED)) | $(RUN_MADGRAPH)
### 
### epem_za.f90:
### 	(echo "e+ e- -> z a"; $(MG_SM)) | $(RUN_MADGRAPH)
### 
### epem_zz.f90:
### 	(echo "e+ e- -> z z"; $(MG_SM)) | $(RUN_MADGRAPH)
### 
### aa_epem.f90:
### 	(echo "a a -> e+ e-"; $(MG_QED)) | $(RUN_MADGRAPH)
### 
### za_epem.f90:
### 	(echo "z a -> e+ e-"; $(MG_SM)) | $(RUN_MADGRAPH)
### 
### za_uub.f90:
### 	(echo "z a -> u u~"; $(MG_SM)) | $(RUN_MADGRAPH)
### 
### za_ddb.f90:
### 	(echo "z a -> d d~"; $(MG_SM)) | $(RUN_MADGRAPH)
### 
### zz_epem.f90:
### 	(echo "z z -> e+ e-"; $(MG_SM)) | $(RUN_MADGRAPH)
### 
### zz_veve.f90:
### 	(echo "z z -> ve ve~"; $(MG_SM)) | $(RUN_MADGRAPH)
### 
### emem_emem.f90:
### 	(echo "e- e- -> e- e-"; $(MG_QED)) | $(RUN_MADGRAPH)
### 
### ema_ema.f90:
### 	(echo "e- a -> e- a"; $(MG_QED)) | $(RUN_MADGRAPH)
### 
### endif
### 
### ########################################################################
### #
### # 5 external lines:
### #
### ########################################################################
### 
### owpwm_zaa_module.f90: $(OMEGA_SM)
### 	$(OMEGA_SM) $(OFLAGS) -scatter "W+ W- -> Z A A" >$@
### 
### owpwm_wpwma_module.f90: $(OMEGA_SM)
### 	$(OMEGA_SM) $(OFLAGS) -scatter "W+ W- -> W+ W- A" >$@
### 
### owpwm_aaa_module.f90: $(OMEGA_SM)
### 	$(OMEGA_SM) $(OFLAGS) -scatter "W+ W- -> A A A" >$@
### 
### oemep_emvewp_module.f90: $(OMEGA_SM)
### 	$(OMEGA_SM) $(OFLAGS) -scatter "e- e+ -> e- nuebar W+" >$@
### 
### oepem_epema_module.f90: $(OMEGA_SM)
### 	$(OMEGA_SM) $(OFLAGS) -scatter "e+ e- -> e+ e- A" >$@
### 
### oemem_emema_module.f90: $(OMEGA_SM)
### 	$(OMEGA_SM) $(OFLAGS) -scatter "e- e- -> e- e- A" >$@
### 
### oepem_aaa_module.f90: $(OMEGA_SM)
### 	$(OMEGA_SM) $(OFLAGS) -scatter "e+ e- -> A A A" >$@
### 
### oepem_zaa_module.f90: $(OMEGA_SM)
### 	$(OMEGA_SM) $(OFLAGS) -scatter "e+ e- -> Z A A" >$@
### 
### oepem_wpwmz_module.f90: $(OMEGA_SM)
### 	$(OMEGA_SM) $(OFLAGS) -scatter "e+ e- -> W+ W- Z" >$@
### 
### oepem_wpwma_module.f90: $(OMEGA_SM)
### 	$(OMEGA_SM) $(OFLAGS) -scatter "e+ e- -> W+ W- A" >$@
### 
### ifneq ($(MADGRAPH),false)
### 
### wpwm_zaa.f90:
### 	(echo "w+ w- -> z a a"; $(MG_SM)) | $(RUN_MADGRAPH)
### 
### wpwm_wpwma.f90:
### 	(echo "w+ w- -> w+ w- a"; $(MG_SM)) | $(RUN_MADGRAPH)
### 
### wpwm_aaa.f90:
### 	(echo "w+ w- -> a a a"; $(MG_SM)) | $(RUN_MADGRAPH)
### 
### emep_emvewp.f90:
### 	(echo "e- e+ -> e- ve~ w+"; $(MG_SM)) | $(RUN_MADGRAPH)
### 
### epem_epema.f90:
### 	(echo "e+ e- -> e+ e- a"; $(MG_SM)) | $(RUN_MADGRAPH)
### 
### emem_emema.f90:
### 	(echo "e- e- -> e- e- a"; $(MG_SM)) | $(RUN_MADGRAPH)
### 
### epem_aaa.f90:
### 	(echo "e+ e- -> a a a"; $(MG_SM)) | $(RUN_MADGRAPH)
### 
### epem_zaa.f90:
### 	(echo "e+ e- -> z a a"; $(MG_SM)) | $(RUN_MADGRAPH)
### 
### epem_wpwmz.f90:
### 	(echo "e+ e- -> w+ w- z"; $(MG_SM)) | $(RUN_MADGRAPH)
### 
### epem_wpwma.f90:
### 	(echo "e+ e- -> w+ w- a"; $(MG_SM)) | $(RUN_MADGRAPH)
### 
### endif
### 
### ########################################################################
### #
### # 6 external lines:
### #
### ########################################################################
### 
### oemep_emvewpa_module.f90: $(OMEGA_SM)
### 	$(OMEGA_SM) $(OFLAGS) -scatter "e- e+ -> e- nuebar W+ A" >$@
### 
### owpwm_uubssb_module.f90:
### 	$(OMEGA_SM) $(OFLAGS) \
### 	  -target:function $(@:_module.f90=) -target:module $(@:.f90=) \
### 	  -scatter "W+ W- -> u ubar s sbar" | \
### 		sed '/! CAVEAT: color factor not known!/s||amp = amp * sqrt (9.0_omega_prec / 1.0_omega_prec) ! CAVEAT: naive color factor|' >$@
### 
### oemep_vevewpwm_module.f90: $(OMEGA_SM)
### 	$(OMEGA_SM) $(OFLAGS) -scatter "e- e+ -> nue nuebar W+ W-" >$@
### 
### oemep_emepwpwm_module.f90: $(OMEGA_SM)
### 	$(OMEGA_SM) $(OFLAGS) -scatter "e- e+ -> e- e+ W+ W-" >$@
### 
### oepem_muvmtavt_module.f90: $(OMEGA_SM)
### 	$(OMEGA_SM) $(OFLAGS) -scatter "e+ e- -> mu- numubar tau+ nutau" >$@
### 
### oepem_epveemve_module.f90: $(OMEGA_SM)
### 	$(OMEGA_SM) $(OFLAGS) -scatter "e+ e- -> e+ nue e- nuebar" >$@
### 
### oepem_mumuaa_module.f90: $(OMEGA_QED)
### 	$(OMEGA_QED) $(OFLAGS) -scatter "e+ e- -> m+ m- A A" >$@
### 
### oepem_epemaa_module.f90: $(OMEGA_QED)
### 	$(OMEGA_QED) $(OFLAGS) -scatter "e+ e- -> e+ e- A A" >$@
### 
### omuem_muemaa_module.f90: $(OMEGA_QED)
### 	$(OMEGA_QED) $(OFLAGS) -scatter "m- e- -> m- e- A A" >$@
### 
### oemem_ememaa_module.f90: $(OMEGA_QED)
### 	$(OMEGA_QED) $(OFLAGS) -scatter "e- e- -> e- e- A A" >$@
### 
### oepem_aaaa_module.f90: $(OMEGA_QED)
### 	$(OMEGA_QED) $(OFLAGS) -scatter "e+ e- -> A A A A" >$@
### 
### oepem_epemepem_module.f90: $(OMEGA_QED)
### 	$(OMEGA_QED) $(OFLAGS) -scatter "e+ e- -> e+ e- e+ e-" >$@
### 
### oepem_wpwmaa_module.f90: $(OMEGA_SM)
### 	$(OMEGA_SM) $(OFLAGS) -scatter "e+ e- -> W+ W- A A" >$@
### 
### oepem_vevebbb_module.f90: $(OMEGA_SM)
### 	$(OMEGA_SM) $(OFLAGS) -scatter "e+ e- -> nue nuebar b bbar" >$@
### 
### ifneq ($(MADGRAPH),false)
### 
### wpwm_uubssb.f90:
### 	(echo "w+ w- -> u u~ s s~"; $(MG_SM)) | $(RUN_MADGRAPH)
### 
### emep_vevewpwm.f90:
### 	(echo "e- e+ -> ve ve~ w+ w-"; $(MG_SM)) | $(RUN_MADGRAPH)
### 
### emep_emepwpwm.f90:
### 	(echo "e- e+ -> e- e+ w+ w-"; $(MG_SM)) | $(RUN_MADGRAPH)
### 
### emep_emvewpa.f90:
### 	(echo "e- e+ -> e- ve~ w+ a"; $(MG_SM)) | $(RUN_MADGRAPH)
### 
### epem_muvmtavt.f90:
### 	(echo "e+ e- -> mu- vm~ ta+ vt"; $(MG_SM)) | $(RUN_MADGRAPH)
### 
### epem_epveemve.f90:
### 	(echo "e+ e- -> e+ ve e- ve~"; $(MG_SM)) | $(RUN_MADGRAPH)
### 
### epem_mumuaa.f90:
### 	(echo "e+ e- -> mu+ mu- a a"; $(MG_QED)) | $(RUN_MADGRAPH)
### 
### epem_epemaa.f90:
### 	(echo "e+ e- -> e+ e- a a"; $(MG_QED)) | $(RUN_MADGRAPH)
### 
### muem_muemaa.f90:
### 	(echo "mu- e- -> mu- e- a a"; $(MG_QED)) | $(RUN_MADGRAPH)
### 
### emem_ememaa.f90:
### 	(echo "e- e- -> e- e- a a"; $(MG_QED)) | $(RUN_MADGRAPH)
### 
### epem_aaaa.f90:
### 	(echo "e+ e- -> a a a a"; $(MG_QED)) | $(RUN_MADGRAPH)
### 
### epem_epemepem.f90:
### 	(echo "e+ e- -> e+ e- e+ e-"; $(MG_QED)) | $(RUN_MADGRAPH)
### 
### epem_wpwmaa.f90:
### 	(echo "e+ e- -> w+ w- a a"; $(MG_SM)) | $(RUN_MADGRAPH)
### 
### epem_vevebbb.f90:
### 	(echo "e+ e- -> ve ve~ b b~"; $(MG_SM)) | $(RUN_MADGRAPH)
### 
### endif
### 
### ########################################################################
### #
### # 7 external lines:
### #
### ########################################################################
### 
### oemep_emveudba_module.f90: $(OMEGA_SM)
### 	$(OMEGA_SM) $(OFLAGS) e- e+ e- nuebar u dbar A >$@
### 
### oepem_veveuubz_module.f90:
### 	$(OMEGA_SM) $(OFLAGS) -scatter "e+ e- -> nue nuebar u ubar Z" >$@
### 
### oepem_muvmtavta_module.f90: $(OMEGA_SM)
### 	$(OMEGA_SM) $(OFLAGS) -scatter "e+ e- -> mu- numubar tau+ nutau A" >$@
### 
### oepem_epemepema_module.f90: $(OMEGA_QED)
### 	$(OMEGA_QED) $(OFLAGS) -scatter "e+ e- -> e+ e- e+ e- A" >$@
### 
### oepem_epemaaa_module.f90: $(OMEGA_QED)
### 	$(OMEGA_QED) $(OFLAGS) -scatter "e+ e- -> e+ e- A A A" >$@
### 
### oepem_aaaaa_module.f90: $(OMEGA_QED)
### 	$(OMEGA_QED) $(OFLAGS) -scatter "e+ e- -> A A A A A" >$@
### 
### oaa_epemaaa_module.f90: $(OMEGA_QED)
### 	$(OMEGA_QED) $(OFLAGS) -scatter "A A -> e+ e- A A A" >$@
### 
### oaa_epemmumua_module.f90: $(OMEGA_QED)
### 	$(OMEGA_QED) $(OFLAGS) -scatter "A A -> e+ e- m+ m- A" >$@
### 
### oaa_epemepema_module.f90: $(OMEGA_QED)
### 	$(OMEGA_QED) $(OFLAGS) -scatter "A A -> e+ e- e+ e- A" >$@
### 
### ifneq ($(MADGRAPH),false)
### 
### epem_veveuubz.f90:
### 	(echo "e+ e- -> ve ve~ u u~ Z"; $(MG_SM)) | $(RUN_MADGRAPH)
### 
### emep_emveudba.f90:
### 	(echo "e- e+ -> e- ve~ u d~ a"; $(MG_SM)) | $(RUN_MADGRAPH)
### 
### epem_muvmtavta.f90:
### 	(echo "e+ e- -> mu- vm~ ta+ vt a"; $(MG_SM)) | $(RUN_MADGRAPH)
### 
### epem_epemaaa.f90:
### 	(echo "e+ e- -> e+ e- a a a"; $(MG_QED)) | $(RUN_MADGRAPH)
### 
### epem_epemepema.f90:
### 	(echo "e+ e- -> e+ e- e+ e- a"; $(MG_QED)) | $(RUN_MADGRAPH)
### 
### epem_aaaaa.f90:
### 	(echo "e+ e- -> a a a a a"; $(MG_QED)) | $(RUN_MADGRAPH)
### 
### aa_epemaaa.f90:
### 	(echo "a a -> e+ e- a a a"; $(MG_QED)) | $(RUN_MADGRAPH)
### 
### aa_epemmumua.f90:
### 	(echo "a a -> e+ e- mu+ mu- a"; $(MG_QED)) | $(RUN_MADGRAPH)
### 
### aa_epemepema.f90:
### 	(echo "a a -> e+ e- e+ e- a"; $(MG_QED)) | $(RUN_MADGRAPH)
### 
### endif
### 
### ########################################################################
### #
### # 8 external lines:
### #
### ########################################################################
### 
### oepem_muvmtavtaa_module.f90: $(OMEGA_SM)
### 	$(OMEGA_SM) $(OFLAGS) -scatter "e+ e- -> mu- numubar tau+ nutau A A" >$@
### 
### oepem_epemaaaa_module.f90: $(OMEGA_QED)
### 	$(OMEGA_QED) $(OFLAGS) -scatter "e+ e- -> e+ e- A A A A" >$@
### 
### oepem_mumutatauub_module.f90: $(OMEGA_SM)
### 	$(OMEGA_SM) $(OFLAGS) -scatter "e+ e- -> mu+ mu- tau+ tau- u ubar" >$@
### 
### oepem_muvmtavtuub_module.f90: $(OMEGA_SM)
### 	$(OMEGA_SM) $(OFLAGS) -scatter "e+ e- -> mu- numubar tau+ nutau u ubar" >$@
### 
### oepem_vevemuvmudb_module.f90: $(OMEGA_SM)
### 	$(OMEGA_SM) $(OFLAGS) -scatter "e+ e- -> nue nuebar mu- numubar u dbar" >$@
### 
### oepem_epvebbbdub_module.f90: $(OMEGA_SM)
### 	$(OMEGA_SM) $(OFLAGS) -scatter "e+ e- -> e+ nue b bbar d ubar" | \
### 		sed '/! CAVEAT: color factor not known!/s||amp = amp * sqrt (9.0_omega_prec / 1.0_omega_prec) ! CAVEAT: naive color factor|' >$@
### 
### single_top_module.f90: $(OMEGA_SM)
### 	$(OMEGA_SM) $(OFLAGS) -scatter "e+ e- -> e+ nue b bbar d ubar" >$@
### 
### single_top_fudged_module.f90: $(OMEGA_SM)
### 	$(OMEGA_SM) -model:fudged_width $(OFLAGS) \
### 	  -scatter "e+ e- -> e+ nue b bbar d ubar" >$@
### 
### single_top_constant_module.f90: $(OMEGA_SM)
### 	$(OMEGA_SM) -model:constant_width $(OFLAGS) \
### 	  -scatter "e+ e- -> e+ nue b bbar d ubar" >$@
### 
### ifneq ($(MADGRAPH),false)
### 
### epem_muvmtavtaa.f90:
### 	(echo "e+ e- -> mu- vm~ ta+ vt a a"; $(MG_SM)) | $(RUN_MADGRAPH)
### 
### epem_epemaaaa.f90:
### 	(echo "e+ e- -> e+ e- a a a a"; $(MG_QED)) | $(RUN_MADGRAPH)
### 
### epem_mumutatauub.f90:
### 	(echo "e+ e- -> mu+ mu- ta+ ta- u u~"; $(MG_SM)) | $(RUN_MADGRAPH)
### 
### epem_muvmtavtuub.f90:
### 	(echo "e+ e- -> mu- vm~ ta+ vt u u~"; $(MG_SM)) | $(RUN_MADGRAPH)
### 
### epem_vevemuvmudb.f90:
### 	(echo "e+ e- -> ve ve~ mu- vm~ u d~"; $(MG_SM)) | $(RUN_MADGRAPH)
### 
### epem_epvebbbdub.f90:
### 	(echo "e+ e- -> e+ ve b b~ d u~"; $(MG_SM)) | $(RUN_MADGRAPH)
### 
### endif
### 
### ########################################################################
### 
### lib$(HELAS).a: $(HELAS).o
### 	ar cr $@ $<
### 
### $(HELAS).o: $(build_srcdir)/$(HELAS).$(FC_SRC_EXT)
### 	$(FC) $(FC_FLAGS) -c -o $@ $<
### 
### clean:
### 	rm -f *.o main[4-9] *~ *.mod *_module*
### 
### purge: purge_omega purge_madlab
### 
### purge_omega:
### 	rm -f $(OMEGA_SRC)
### 
### purge_madlab:
### 	rm -f $(MADGRAPH_SRC)
### 
### compare:
### 	$(MAKE) -n -W $(OMEGA_QED)  -W $(OMEGA_SM) \
### 	        | egrep '$(OMEGA_QED)|$(OMEGA_SM)' \
### 		| sed -e 's/>/>tmp.compare; diff -I"^!" -u /' -e 's/$$/ tmp.compare/' | sh
### 
### MADGRAPH_HEADER = \
### 	 echo "      use $(HELAS)"; \
### 	 echo "      use omega_parameters_madgraph"; \
### 	 echo "      implicit none"; \
### 	 echo "      integer,parameter :: D = selected_real_kind(14,100)"; \
### 	 echo "      contains"; \
### 	 sed -e 'y/abcdefghijklmnopqrstuvwxyz/ABCDEFGHIJKLMNOPQRSTUVWXYZ/' \
### 	     -e 's/END *$$/END FUNCTION/' \
### 	     -e '/WRITE/s//! WRITE/' \
### 	     -e '/INTERFACE/,/END INTERFACE/s/^/!!! /' \
### 	     -e '/GLOBAL VARIABLES/,/COLOR DATA/s/^/!!! /'
### 
### madgraph4.f90: $(MADGRAPH_SRC4) Makefile
### 	(echo "      module madgraph4"; \
### 	 $(MADGRAPH_HEADER) $(MADGRAPH_SRC4); \
### 	 echo "      end module madgraph4" ) >$@
### 
### madgraph5.f90: $(MADGRAPH_SRC5) Makefile
### 	(echo "      module madgraph5"; \
### 	 $(MADGRAPH_HEADER) $(MADGRAPH_SRC5); \
### 	 echo "      end module madgraph5" ) >$@
### 
### madgraph6.f90: $(MADGRAPH_SRC6) Makefile
### 	(echo "      module madgraph6"; \
### 	 $(MADGRAPH_HEADER) $(MADGRAPH_SRC6); \
### 	 echo "      end module madgraph6" ) >$@
### 
### madgraph7.f90: $(MADGRAPH_SRC7) Makefile
### 	(echo "      module madgraph7"; \
### 	 $(MADGRAPH_HEADER) $(MADGRAPH_SRC7); \
### 	 echo "      end module madgraph7" ) >$@
### 
### madgraph8.f90: $(MADGRAPH_SRC8) Makefile
### 	(echo "      module madgraph8"; \
### 	 $(MADGRAPH_HEADER) $(MADGRAPH_SRC8); \
### 	 echo "      end module madgraph8" ) >$@
### 
### madgraphx.f90: $(MADGRAPH_SRCX) Makefile
### 	(echo "      module madgraphx"; \
### 	 $(MADGRAPH_HEADER) $(MADGRAPH_SRCX); \
### 	 echo "      end module madgraphx" ) >$@
### 
### omega_amplitudes4.f90: Makefile
### 	(echo "module $(@:.f90=)"; \
### 	 for m in $(OMEGA_SRC4:.f90=); do echo "  use $$m"; done; \
### 	 echo "  public"; \
### 	 echo "end module $(@:.f90=)") >$@
### 
### omega_amplitudes5.f90: Makefile
### 	(echo "module $(@:.f90=)"; \
### 	 for m in $(OMEGA_SRC5:.f90=); do echo "  use $$m"; done; \
### 	 echo "  public"; \
### 	 echo "end module $(@:.f90=)") >$@
### 
### omega_amplitudes6.f90: Makefile
### 	(echo "module $(@:.f90=)"; \
### 	 for m in $(OMEGA_SRC6:.f90=); do echo "  use $$m"; done; \
### 	 echo "  public"; \
### 	 echo "end module $(@:.f90=)") >$@
### 
### omega_amplitudes7.f90: Makefile
### 	(echo "module $(@:.f90=)"; \
### 	 for m in $(OMEGA_SRC7:.f90=); do echo "  use $$m"; done; \
### 	 echo "  public"; \
### 	 echo "end module $(@:.f90=)") >$@
### 
### omega_amplitudes8.f90: Makefile
### 	(echo "module $(@:.f90=)"; \
### 	 for m in $(OMEGA_SRC8:.f90=); do echo "  use $$m"; done; \
### 	 echo "  public"; \
### 	 echo "end module $(@:.f90=)") >$@
### 
### omega_amplitudesx.f90: Makefile
### 	(echo "module $(@:.f90=)"; \
### 	 for m in $(OMEGA_SRCX:.f90=); do echo "  use $$m"; done; \
### 	 echo "  public"; \
### 	 echo "end module $(@:.f90=)") >$@
### 
### omega_amplitudest.f90: Makefile
### 	(echo "module $(@:.f90=)"; \
### 	 for m in $(OMEGA_SRCT:.f90=); do echo "  use $$m"; done; \
### 	 echo "  public"; \
### 	 echo "end module $(@:.f90=)") >$@
### 
### omega_amplitudes.o: $(OMEGA_OBJ)
### 
### madgraph.o: $(auxdir)/kinds.o $(build_srcdir)/omega_parameters_madgraph.o
### 
### main4.o: $(OBJS4) lib$(HELAS).a $(build_libdir)/libomega95.a 
### main5.o: $(OBJS5) lib$(HELAS).a $(build_libdir)/libomega95.a 
### main6.o: $(OBJS6) lib$(HELAS).a $(build_libdir)/libomega95.a 
### main7.o: $(OBJS7) lib$(HELAS).a $(build_libdir)/libomega95.a 
### main8.o: $(OBJS8) lib$(HELAS).a $(build_libdir)/libomega95.a 
### mainx.o: $(OBJSX) lib$(HELAS).a $(build_libdir)/libomega95.a 
### maint.o: $(OBJST) $(build_libdir)/libomega95.a 
### 
### ########################################################################
### 
### $(build_libdir)/libomega95.a:
### 	$(MAKE) -C $(build_srcdir) $(build_libdir)/libomega95.a
### 
### $(build_libdir)/libomega95_tools.a:
### 	$(MAKE) -C $(build_tooldir) $(build_libdir)/libomega95_tools.a
### 
### ########################################################################
### 
### parameters.MSSM_4.omega.o: parameters.MSSM_4.omega.f90
### 	$(FC) $(FC_FLAGS) -c -o $@ $<
### 
### parameters.MSSM_4.omega.f90: $(top_srcdir)/../../conf/models/parameters.MSSM_4.omega.f90
### 	cp $(top_srcdir)/../../conf/models/parameters.MSSM_4.omega.f90 $(srcdir)=======
