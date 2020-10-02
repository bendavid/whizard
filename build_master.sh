#! /bin/sh
# BUILD MASTER for WHIZARD (and enabling subpackages)
########################################################################
#
# Copyright (C) 1999-2020 by 
#     Wolfgang Kilian <kilian@physik.uni-siegen.de>
#     Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
#     Juergen Reuter <juergen.reuter@desy.de>
#     with contributions from
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

option=MASTER
case "$1" in
    WHIZARD|VAMP|OMEGA|CIRCE1|CIRCE2)   option=$1;;
    "")  option="WHIZARD";;
    *)  echo "Possible targets are: MASTER|OMEGA|VAMP|CIRCE1|CIRCE2"; exit 2
esac	

echo "#################################################"
echo "Deploying WHIZARD master package $option"
echo "#################################################"

remove_tags () {
    sed "/\#\#\# $1 {{{/,/$1 }}}/ { 1 { s/^.*//
					b
				      }
				    d
				  }" $2 > $3
}

# Creating main configure.ac and Makefile.am for WHIZARD and subpackages

case "$option" in
    WHIZARD)
	sed -e "s/\#\#\#SUBDIRS\#\#\#/circe1 circe2 omega vamp mcfio stdhep tauola pythia6 src python share tests scripts/g" -e "/\#\#\# ONLY_FULL/d" Makefile.am.in > Makefile.am.tmp1
	remove_tags ONLY_OMEGA Makefile.am.tmp1 Makefile.am.tmp2
	rm -f Makefile.am.tmp1
	mv -f Makefile.am.tmp2 Makefile.am
	sed -e "s/XXXWHIZARDXXX/WHIZARD/g" -e "/\#\#\# ONLY_/d" configure.ac.in > configure.ac.tmp
	mv -f configure.ac.tmp configure.ac;;
    CIRCE1)
	sed -e "s/\#\#\#SUBDIRS\#\#\#/circe1/g" Makefile.am.in > Makefile.am.tmp1
	remove_tags ONLY_OMEGA Makefile.am.tmp1 Makefile.am.tmp2
	rm -f Makefile.am.tmp1
	remove_tags ONLY_FULL Makefile.am.tmp2 Makefile.am.tmp3
	rm -f Makefile.am.tmp2
	mv -f Makefile.am.tmp3 Makefile.am
	sed -e "s/XXXWHIZARDXXX/WHIZARD_CIRCE1/g" -e "s/src\/basics\/kinds\.f90/circe1\/src\/kinds\.f90/g" -e "/\#\#\# ONLY_CIRCE1_AND_FULL/d" configure.ac.in > configure.ac.tmp1
	remove_tags ONLY_FULL configure.ac.tmp1 configure.ac.tmp2
	rm -f configure.ac.tmp1
	remove_tags ONLY_CIRCE2_AND_FULL configure.ac.tmp2 configure.ac.tmp3
	rm -f configure.ac.tmp2	
	remove_tags ONLY_VAMP_AND_FULL configure.ac.tmp3 configure.ac.tmp4
	rm -f configure.ac.tmp3
	remove_tags ONLY_OMEGA_AND_FULL configure.ac.tmp4 configure.ac.tmp5
	rm -f configure.ac.tmp4
	mv -f configure.ac.tmp5 configure.ac;;
    CIRCE2)
	sed -e "s/\#\#\#SUBDIRS\#\#\#/circe2/g" Makefile.am.in > Makefile.am.tmp1
	remove_tags ONLY_OMEGA Makefile.am.tmp1 Makefile.am.tmp2
	rm -f Makefile.am.tmp1
	remove_tags ONLY_FULL Makefile.am.tmp2 Makefile.am.tmp3
	rm -f Makefile.am.tmp2
	mv -f Makefile.am.tmp3 Makefile.am
	sed -e "s/XXXWHIZARDXXX/WHIZARD_CIRCE2/g" -e "s/src\/basics\/kinds\.f90/circe2\/src\/kinds\.f90/g" -e "/\#\#\# ONLY_CIRCE2_AND_FULL/d" configure.ac.in > configure.ac.tmp1
	remove_tags ONLY_FULL configure.ac.tmp1 configure.ac.tmp2
	rm -f configure.ac.tmp1
	remove_tags ONLY_VAMP_AND_FULL configure.ac.tmp2 configure.ac.tmp3
	rm -f configure.ac.tmp2
	remove_tags ONLY_CIRCE1_AND_FULL configure.ac.tmp3 configure.ac.tmp4
	rm -f configure.ac.tmp3
	remove_tags ONLY_OMEGA_AND_FULL configure.ac.tmp4 configure.ac.tmp5
	rm -f configure.ac.tmp4
	mv -f configure.ac.tmp5 configure.ac;;
    OMEGA)
	sed -e "s/\#\#\#SUBDIRS\#\#\#/omega/g" -e "/\#\#\# ONLY_OMEGA/d" Makefile.am.in > Makefile.am.tmp1
	remove_tags ONLY_FULL Makefile.am.tmp1 Makefile.am.tmp2
	rm -f Makefile.am.tmp1
	mv -f Makefile.am.tmp2 Makefile.am
	sed -e "s/XXXWHIZARDXXX/WHIZARD_OMEGA/g" -e "s/src\/basics\/kinds\.f90/omega\/src\/kinds\.f90/g" -e "/\#\#\# ONLY_OMEGA_AND_FULL/d" configure.ac.in > configure.ac.tmp1
	remove_tags ONLY_FULL configure.ac.tmp1 configure.ac.tmp2
	rm -f configure.ac.tmp1
	remove_tags ONLY_VAMP_AND_FULL configure.ac.tmp2 configure.ac.tmp3
	rm -f configure.ac.tmp2
	remove_tags ONLY_CIRCE1_AND_FULL configure.ac.tmp3 configure.ac.tmp4
	rm -f configure.ac.tmp3
	remove_tags ONLY_CIRCE2_AND_FULL configure.ac.tmp4 configure.ac.tmp5
	rm -f configure.ac.tmp4
	mv -f configure.ac.tmp5 configure.ac;;
    VAMP)
	sed -e "s/\#\#\#SUBDIRS\#\#\#/vamp/g" Makefile.am.in > Makefile.am.tmp1
	remove_tags ONLY_OMEGA Makefile.am.tmp1 Makefile.am.tmp2
	rm -f Makefile.am.tmp1
	remove_tags ONLY_FULL Makefile.am.tmp2 Makefile.am.tmp3
	rm -f Makefile.am.tmp2
	mv -f Makefile.am.tmp3 Makefile.am
	sed -e "s/XXXWHIZARDXXX/WHIZARD_VAMP/g" -e "s/src\/basics\/kinds\.f90/vamp\/src\/kinds\.f90/g" configure.ac.in > configure.ac.tmp1
	remove_tags ONLY_FULL configure.ac.tmp1 configure.ac.tmp2
	rm -f configure.ac.tmp1
	remove_tags ONLY_CIRCE1_AND_FULL configure.ac.tmp2 configure.ac.tmp3
	rm -f configure.ac.tmp2
	remove_tags ONLY_CIRCE2_AND_FULL configure.ac.tmp3 configure.ac.tmp4
	rm -f configure.ac.tmp3
	remove_tags ONLY_OMEGA_AND_FULL configure.ac.tmp4 configure.ac.tmp5
	rm -f configure.ac.tmp4
	sed -e "/\#\#\# ONLY_VAMP_AND_FULL/d" configure.ac.tmp5 > configure.ac.tmp6
	rm -f configure.ac.tmp5
	mv -f configure.ac.tmp6 configure.ac;;	
esac
	
