# Makefile.am -- Makefile for O'Mega within and without WHIZARD
# $Id: Makefile.am 6465 2015-01-10 15:22:31Z jr_reuter $
##
## Process this file with automake to produce Makefile.in
##
########################################################################
#
# Copyright (C) 1999-2015 by 
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

### auxdir = $(top_srcdir)/src/misc
### 
### build_bindir = $(top_srcdir)/bin
### build_libdir = $(top_srcdir)/lib
### build_srcdir = $(top_srcdir)/src
### build_extdir = $(top_srcdir)/extensions/people/jr
### 
### OMEGA_QED = $(build_bindir)/f90_QED.opt
### OMEGA_SM = $(build_bindir)/f90_SM.opt
### # OMEGA_SM = $(build_bindir)/f90_SM4.opt
### OMEGA_SMG = $(build_bindir)/f90_SM_clones.opt
### OMEGA_SAGT = $(build_extdir)/f90_SAGT.opt
### 
### # OMEGA_QED = $(build_bindir)/f90Maj_QED.opt
### # OMEGA_SM = $(build_bindir)/f90Maj_SM.opt
### # OMEGA_SMG = $(build_bindir)/f90Maj_SM_clones.opt
### 
### # OMEGA_QED = $(build_bindir)/helas_QED.opt
### # OMEGA_SM = $(build_bindir)/helas_SM.opt
### # OMEGA_SMG = false
### 
### OFLAGS = -target:function $(@:_module.f90=) -target:module $(@:.f90=) \
### 	-target:parameter_module omega_parameters_jr -old-interface
### 
### F132 = -132
### FCOPT = $(F132) -I$(build_libdir) -O3 
### # FCOPT = -I $(build_libdir) -pg -O3 -Oassumed=contig
### 
### # FCOPT = -M $(build_libdir) -Am -Nmaxserious=1
### # FCOPT = -M $(build_libdir) -Am -O3 -x - -Nmaxserious=1
### # FCOPT = -M $(build_libdir) -Am -g -Nmaxserious=1
### # FCOPT = -M $(build_libdir) -Am -O0 -Nmaxserious=1
### 
### CPIF = @CPIF@
### 
### MG_QED = echo 0; echo; echo; echo
### MG_SM = echo 0; echo yes; echo; echo
### 
### LIBS = -L$(build_libdir) -lomega95 -L. 
### 
### 
### TEST_OBJS = $(addprefix $(top_srcdir)/tools/,kinematics.o tao_random_numbers.o rambo.o testbed.o) \
### 	omega_parameters_jr.o
### 
### OMEGA_SRC4 = ofa_fa_module.f90 off_ff_module.f90 oaa_ff_module.f90 \
### 	off_aa_module.f90
### 
### OMEGA_SRC5 = 
### 
### OMEGA_SRC6 = oaa_ffpp_module.f90 obb_ffff_module.f90 off_ppaa_module.f90 \
### 	oaa_pppp_module.f90 oaa_ffff_module.f90 
### 
### OMEGA_SRC8 = oaa_ffffpp_module.f90 
### 
### OMEGA_SRC = $(OMEGA_SRC4) $(OMEGA_SRC5) $(OMEGA_SRC6) $(OMEGA_SRC8)
### 
### OMEGA_OBJ4 = $(OMEGA_SRC4:.f90=.o)
### OMEGA_OBJ5 = $(OMEGA_SRC5:.f90=.o)
### OMEGA_OBJ6 = $(OMEGA_SRC6:.f90=.o)
### OMEGA_OBJ8 = $(OMEGA_SRC8:.f90=.o)
### OMEGA_OBJ = $(OMEGA_SRC:.f90=.o)
### 
### all: main4 main6 main8
### 
### OBJS4 = $(OMEGA_OBJ4) omega_amplitudes4.o $(TEST_OBJS)
### 
### OBJS5 = $(OMEGA_OBJ5) omega_amplitudes5.o $(TEST_OBJS)
### 
### OBJS6 = $(OMEGA_OBJ6) omega_amplitudes6.o $(TEST_OBJS)
### 
### OBJS8 = $(OMEGA_OBJ8) omega_amplitudes8.o $(TEST_OBJS)
### 
### %.o: %.$(FC_SRC_EXT)
### 	$(FC) $(FCOPT) -c -o $@ $(TEST_OBJS) $<
### 
### omega_parameters_jr.o: omega_parameters_jr.$(FC_SRC_EXT)
### 	$(FC) $(FCOPT) -c -o $@ $<
### 
### omega_parameters_jr.$(FC_SRC_EXT): $(OMEGA_SAGT)
### 	$(OMEGA_SAGT) -params > $@ 
### 
### %.$(FC_SRC_EXT): %.f90
### if FC_IMPURE
### 	sed '/^[ 	]*pure[	 ]/s/pure[	 ]//' $< | $(CPIF) $@
### else
### 	$(CPIF) $@ < $<
### endif
### 
### main4: main4.o
### 	$(FC) $(FCOPT) -o $@ $(OBJS4) main4.o $(LIBS)
### 
### main5: main5.o
### 	$(FC) $(FCOPT) -o $@ $(OBJS5) main5.o $(LIBS)
### 
### main6: main6.o
### 	$(FC) $(FCOPT) -o $@ $(OBJS6) main6.o $(LIBS)
### 
### main8: main8.o
### 	$(FC) $(FCOPT) -o $@ $(OBJS8) main8.o $(LIBS)
### 
### 
### # 4 external lines
### 
### oaa_ff_module.f90: $(OMEGA_SAGT) $(TEST_OBJS)
### 	$(OMEGA_SAGT) $(OFLAGS) -scatter "a a -> f f" >$@
### 
### off_aa_module.f90: $(OMEGA_SAGT) $(TEST_OBJS)
### 	$(OMEGA_SAGT) $(OFLAGS) -scatter "f f -> a a" >$@
### 
### ofa_fa_module.f90: $(OMEGA_SAGT) $(TEST_OBJS)
### 	$(OMEGA_SAGT) $(OFLAGS) -scatter "f a -> f a" >$@
### 
### off_ff_module.f90: $(OMEGA_SAGT) $(TEST_OBJS)
### 	$(OMEGA_SAGT) $(OFLAGS) -scatter "f f -> f f" >$@  
### 
### oaa_ffff_module.f90: $(OMEGA_SAGT) $(TEST_OBJS)
### 	$(OMEGA_SAGT) $(OFLAGS) -scatter "a a -> f f f f" >$@
### 
### oaa_pppp_module.f90: $(OMEGA_SAGT) $(TEST_OBJS)
### 	$(OMEGA_SAGT) $(OFLAGS) -scatter "a a -> phino phino phino phino" >$@
### 
### oaa_ffpp_module.f90: $(OMEGA_SAGT) $(TEST_OBJS)
### 	$(OMEGA_SAGT) $(OFLAGS) -scatter "a a -> f f phino phino" >$@
### 
### off_ppaa_module.f90: $(OMEGA_SAGT) $(TEST_OBJS)
### 	$(OMEGA_SAGT) $(OFLAGS) -scatter "f f -> phino phino a a" >$@
### 
### obb_ffff_module.f90: $(OMEGA_SAGT) $(TEST_OBJS)
### 	$(OMEGA_SAGT) $(OFLAGS) -scatter "b b -> f f f f" >$@
### 
### oaa_ffffpp_module.f90: $(OMEGA_SAGT) $(TEST_OBJS)
### 	$(OMEGA_SAGT) $(OFLAGS) -scatter "a a -> f f f f phino phino" >$@
### 
### ########################################################################
### 
### lib$(HELAS).a: $(HELAS).o
### 	ar cr $@ $<
### 
### $(HELAS).o: $(HELAS).$(FC_SRC_EXT)
### 	$(FC) $(FCOPT) -c -o $@ $<
### 
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
### omega_amplitudes4.f90: Makefile
### 	(echo "module $(@:.f90=)"; \
### 	 for m in $(OMEGA_SRC4:.f90=); do echo "  use $$m"; done; \
### 	 echo "  public"; \
### 	 echo "end module $(@:.f90=)") >$@
### 
### omega_amplitudes5.f90: Makefile
### 	(echo "module $(@:.f90=)"; \
### 	for m in $(OMEGA_SRC5:.f90=); do echo "  use $$m"; done; \
### 	echo "  public"; \
### 	echo "end module $(@:.f90=)") >$@
### 
### omega_amplitudes6.f90: Makefile
### 	(echo "module $(@:.f90=)"; \
### 	for m in $(OMEGA_SRC6:.f90=); do echo "  use $$m"; done; \
### 	echo "  public"; \
### 	echo "end module $(@:.f90=)") >$@    
### 
### omega_amplitudes8.f90: Makefile
### 	(echo "module $(@:.f90=)"; \
### 	for m in $(OMEGA_SRC8:.f90=); do echo "  use $$m"; done; \
### 	echo "  public"; \
### 	echo "end module $(@:.f90=)") >$@
### 
### kinematics.o: $(auxdir)/kinds.o
### rambo.o: $(auxdir)/kinds.o kinematics.o tao_random_numbers.o
### testbed.o: $(auxdir)/kinds.o rambo.o
### 
### $(auxdir)/kinds.o: $(auxdir)/kinds.f90
### 	$(MAKE) -C $(auxdir) 
### 
### $(build_libdir)/libomega95.a:
### 	$(MAKE) -C $(build_libdir) libomega95.a
### 
### omega_amplitudes.o: $(OMEGA_OBJ)
### 
### main4.o: $(OBJS4) $(build_libdir)/libomega95.a
### 
### main5.o: $(OBJS5) $(build_libdir)/libomega95.a
### 
### main6.o: $(OBJS6) $(build_libdir)/libomega95.a
### 
### main8.o: $(OBJS8) $(build_libdir)/libomega95.a
