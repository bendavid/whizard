! circe2d.f90 -- beam spectra for linear colliders and photon colliders
! $Id: circe2.nw,v 1.56 2002/10/14 10:12:06 ohl Exp $
! Copyright (C) 2001-2014 by 
!      Wolfgang Kilian <kilian@physik.uni-siegen.de>
!      Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
!      Juergen Reuter <juergen.reuter@desy.de>
!      Christian Speckner <cnspeckn@googlemail.com>
!
! Circe2 is free software; you can redistribute it and/or modify it
! under the terms of the GNU General Public License as published by
! the Free Software Foundation; either version 2, or (at your option)
! any later version.
!
! Circe2 is distributed in the hope that it will be useful, but
! WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License
! along with this program; if not, write to the Free Software
! Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

  subroutine cir2lb (design, roots, ierror)
     implicit none
    integer, parameter :: single = &
      & selected_real_kind (precision(1.), range(1.))
    integer, parameter :: double = &
      & selected_real_kind (precision(1._single) + 1, range(1._single) + 1)    
    character(len=*), intent(in) :: design
    real(kind=double) :: roots
    integer :: ierror
      integer, parameter :: NBMAX = 100, NCMAX = 36
      integer, parameter :: POLAVG = 1, POLHEL = 2, POLGEN = 3
        integer, parameter :: NBMMAX = 1
              real(kind=double), dimension(0:NBMAX*NBMAX,NCMAX,NBMMAX) :: bdwgt
              common /cir2cd/ bdwgt
              real(kind=double), dimension(NBMAX,NBMAX,NCMAX,NBMMAX) :: bdval
              common /cir2cd/ bdval
              real(kind=double), dimension(0:NBMAX,NCMAX,NBMMAX) :: bdxb1
              real(kind=double), dimension(0:NBMAX,NCMAX,NBMMAX) :: bdxb2
              common /cir2cd/ bdxb1, bdxb2
              real(kind=double), dimension(NCMAX,NBMMAX) :: bdlumi
              common /cir2cd/ bdlumi
              real(kind=double), dimension(0:NCMAX,NBMMAX) :: bdcwgt
              common /cir2cd/ bdcwgt
              real(kind=double), dimension(0:NBMAX,NCMAX,NBMMAX) :: bdyb1
              real(kind=double), dimension(0:NBMAX,NCMAX,NBMMAX) ::  bdyb2
              common /cir2cd/ bdyb1, bdyb2
              real(kind=double), dimension(NBMAX,NCMAX,NBMMAX) :: bdalf1
              real(kind=double), dimension(NBMAX,NCMAX,NBMMAX) :: bdalf2
              real(kind=double), dimension(NBMAX,NCMAX,NBMMAX) :: bdxi1
              real(kind=double), dimension(NBMAX,NCMAX,NBMMAX) :: bdxi2
              real(kind=double), dimension(NBMAX,NCMAX,NBMMAX) :: bdeta1
              real(kind=double), dimension(NBMAX,NCMAX,NBMMAX) :: bdeta2
              real(kind=double), dimension(NBMAX,NCMAX,NBMMAX) :: bda1
              real(kind=double), dimension(NBMAX,NCMAX,NBMMAX) :: bda2
              real(kind=double), dimension(NBMAX,NCMAX,NBMMAX) :: bdb1
              real(kind=double), dimension(NBMAX,NCMAX,NBMMAX) :: bdb2
              common /cir2cd/ bdalf1, bdxi1, bdeta1, bda1, bdb1
              common /cir2cd/ bdalf2, bdxi2, bdeta2, bda2, bdb2
              integer :: bdnb
              common /cir2cd/ bdnb
              integer, dimension(NCMAX,NBMMAX) :: bdnb1, bdnb2
              common /cir2cd/ bdnb1, bdnb2
              logical, dimension(NCMAX,NBMMAX) :: bdtria
              common /cir2cd/ bdtria
              integer, dimension(NBMMAX) :: bdnc
              common /cir2cd/ bdnc
              integer, dimension(NCMAX,NBMMAX) :: bdpid1, bdpid2
              integer, dimension(NCMAX,NBMMAX) :: bdpol1, bdpol2
              common /cir2cd/ bdpid1, bdpol1, bdpid2, bdpol2
              integer, dimension(NBMAX,NCMAX,NBMMAX) :: bdmap1, bdmap2
              common /cir2cd/ bdmap1, bdmap2
              character(len=6), dimension(NBMMAX) :: bddsgn
              common /cir2cd/ bddsgn
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    external cir2bd
    integer :: ib, ic, i1, i2
    integer :: loaded
    loaded = 0
    do ib = 1, bdnb
       if (design .eq. bddsgn(ib)) then
          print *, 'circe2: CIR2LB not available yet!'
          loaded = loaded + 1
       end if
    end do
    if (loaded .gt. 0) then           
       ierror = 0
    else
       write (*, '(A)') 'circe2: no matching design'
       ierror = -1
    end if
  end subroutine cir2lb


    block data cir2bd
        implicit none
      integer, parameter :: single = &
        & selected_real_kind (precision(1.), range(1.))
      integer, parameter :: double = &
        & selected_real_kind (precision(1._single) + 1, range(1._single) + 1)    
                  integer, parameter :: NBMAX = 100, NCMAX = 36
                  integer, parameter :: NBMMAX = 1
                  integer, parameter :: POLAVG = 1, POLHEL = 2, POLGEN = 3
                real(kind=double), dimension(0:NBMAX*NBMAX,NCMAX) :: wgt
                real(kind=double), dimension(NBMAX,NBMAX,NCMAX) :: val
                real(kind=double), dimension(0:NBMAX,NCMAX) :: xb1, xb2
                real(kind=double), dimension(NCMAX) :: lumi
                real(kind=double), dimension(0:NCMAX) :: cwgt
                real(kind=double), dimension(0:NBMAX,NCMAX) :: yb1, yb2
                real(kind=double), dimension(NBMAX,NCMAX) :: alpha1, alpha2
                real(kind=double), dimension(NBMAX,NCMAX) :: xi1, xi2
                real(kind=double), dimension(NBMAX,NCMAX) :: eta1, eta2
                real(kind=double), dimension(NBMAX,NCMAX) :: a1, a2
                real(kind=double), dimension(NBMAX,NCMAX) :: b1, b2
            common /cir2cm/ wgt
            common /cir2cm/ val
            common /cir2cm/ xb1, xb2
            common /cir2cm/ lumi
            common /cir2cm/ cwgt
            common /cir2cm/ yb1, yb2
            common /cir2cm/ alpha1, xi1, eta1, a1, b1
            common /cir2cm/ alpha2, xi2, eta2, a2, b2
                integer, dimension(NCMAX) :: nb1, nb2
                logical, dimension(NCMAX) :: triang
                integer :: nc
                integer, dimension(NCMAX) :: pid1, pid2
                integer, dimension(NCMAX) :: pol1, pol2
                integer, dimension(NBMAX,NCMAX) :: map1, map2
                integer :: polspt
            common /cir2cm/ nb1, nb2
            common /cir2cm/ triang
            common /cir2cm/ nc
            common /cir2cm/ pid1, pol1, pid2, pol2
            common /cir2cm/ map1, map2
            common /cir2cm/ polspt
            save /cir2cm/
                real(kind=double), dimension(0:NBMAX*NBMAX,NCMAX,NBMMAX) :: bdwgt
                common /cir2cd/ bdwgt
                real(kind=double), dimension(NBMAX,NBMAX,NCMAX,NBMMAX) :: bdval
                common /cir2cd/ bdval
                real(kind=double), dimension(0:NBMAX,NCMAX,NBMMAX) :: bdxb1
                real(kind=double), dimension(0:NBMAX,NCMAX,NBMMAX) :: bdxb2
                common /cir2cd/ bdxb1, bdxb2
                real(kind=double), dimension(NCMAX,NBMMAX) :: bdlumi
                common /cir2cd/ bdlumi
                real(kind=double), dimension(0:NCMAX,NBMMAX) :: bdcwgt
                common /cir2cd/ bdcwgt
                real(kind=double), dimension(0:NBMAX,NCMAX,NBMMAX) :: bdyb1
                real(kind=double), dimension(0:NBMAX,NCMAX,NBMMAX) ::  bdyb2
                common /cir2cd/ bdyb1, bdyb2
                real(kind=double), dimension(NBMAX,NCMAX,NBMMAX) :: bdalf1
                real(kind=double), dimension(NBMAX,NCMAX,NBMMAX) :: bdalf2
                real(kind=double), dimension(NBMAX,NCMAX,NBMMAX) :: bdxi1
                real(kind=double), dimension(NBMAX,NCMAX,NBMMAX) :: bdxi2
                real(kind=double), dimension(NBMAX,NCMAX,NBMMAX) :: bdeta1
                real(kind=double), dimension(NBMAX,NCMAX,NBMMAX) :: bdeta2
                real(kind=double), dimension(NBMAX,NCMAX,NBMMAX) :: bda1
                real(kind=double), dimension(NBMAX,NCMAX,NBMMAX) :: bda2
                real(kind=double), dimension(NBMAX,NCMAX,NBMMAX) :: bdb1
                real(kind=double), dimension(NBMAX,NCMAX,NBMMAX) :: bdb2
                common /cir2cd/ bdalf1, bdxi1, bdeta1, bda1, bdb1
                common /cir2cd/ bdalf2, bdxi2, bdeta2, bda2, bdb2
                integer :: bdnb
                common /cir2cd/ bdnb
                integer, dimension(NCMAX,NBMMAX) :: bdnb1, bdnb2
                common /cir2cd/ bdnb1, bdnb2
                logical, dimension(NCMAX,NBMMAX) :: bdtria
                common /cir2cd/ bdtria
                integer, dimension(NBMMAX) :: bdnc
                common /cir2cd/ bdnc
                integer, dimension(NCMAX,NBMMAX) :: bdpid1, bdpid2
                integer, dimension(NCMAX,NBMMAX) :: bdpol1, bdpol2
                common /cir2cd/ bdpid1, bdpol1, bdpid2, bdpol2
                integer, dimension(NBMAX,NCMAX,NBMMAX) :: bdmap1, bdmap2
                common /cir2cd/ bdmap1, bdmap2
                character(len=6), dimension(NBMMAX) :: bddsgn
                common /cir2cd/ bddsgn
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      data bdnb /0/
      data bddsgn(1) /'TESLA '/
    end block data cir2bd

