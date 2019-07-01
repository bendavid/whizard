! WHIZARD 2.2.4 Feb 06 2015
! 
! Copyright (C) 1999-2015 by 
!     Wolfgang Kilian <kilian@physik.uni-siegen.de>
!     Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
!     Juergen Reuter <juergen.reuter@desy.de>
!     
!     with contributions from
!     Fabian Bach <fabian.bach@desy.de>
!     Christian Speckner <cnspeckn@googlemail.com> 
!     Christian Weiss <christian.weiss@desy.de>
!     and Hans-Werner Boschmann, Felix Braam, 
!     Sebastian Schmidt, Daniel Wiesler 
!
! WHIZARD is free software; you can redistribute it and/or modify it
! under the terms of the GNU General Public License as published by 
! the Free Software Foundation; either version 2, or (at your option)
! any later version.
!
! WHIZARD is distributed in the hope that it will be useful, but
! WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the 
! GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License
! along with this program; if not, write to the Free Software
! Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This file has been stripped of most comments.  For documentation, refer
! to the source 'whizard.nw'

module shower_topythia

  use kinds, only: default
  use physics_defs
  use lorentz
  use shower_base
  use shower_partons
  use shower_core

  implicit none
  private

  public :: shower_converttopythia

contains

  subroutine shower_converttopythia (shower)
    IMPLICIT DOUBLE PRECISION(A-H, O-Z)
    IMPLICIT INTEGER(I-N)
    !!!    C...  Commonblocks.
    COMMON/PYJETS/N,NPAD,K(4000,5),P(4000,5),V(4000,5)
    COMMON/PYDAT1/MSTU(200),PARU(200),MSTJ(200),PARJ(200)
    COMMON/PYDAT2/KCHG(500,4),PMAS(500,4),PARF(2000),VCKM(4,4)
    COMMON/PYDAT3/MDCY(500,3),MDME(8000,2),BRAT(8000),KFDP(8000,5)
    COMMON/PYSUBS/MSEL,MSELPD,MSUB(500),KFIN(2,-40:40),CKIN(200)
    COMMON/PYPARS/MSTP(200),PARP(200),MSTI(200),PARI(200)
    SAVE /PYJETS/,/PYDAT1/,/PYDAT2/,/PYDAT3/,/PYSUBS/,/PYPARS/

    type(shower_t), intent(in) :: shower
    type(parton_t), pointer :: pp, ppparent
    integer :: i, j, nz

    !!! currently only works for one interaction

    do i = 1, 2
       !!! get history of the event
       pp => shower%interactions(1)%i%partons(i)%p
       !!! add these partons to the event record
       if (associated (pp%initial)) then
          !!! add hadrons
          K(i,1) = GLUON
          K(i,2) = pp%initial%type
          K(i,3) = 0
          P(i,1) = pp%initial%momentum%p(1)
          P(i,2) = pp%initial%momentum%p(2)
          P(i,3) = pp%initial%momentum%p(3)
          P(i,4) = pp%initial%momentum%p(0)
          P(I,5) = pp%initial%momentum**2
          !!! add partons emitted by the hadron
          ppparent => pp
          do while (associated (ppparent%parent))
             if (parton_is_hadron (ppparent%parent)) then
                exit
             else
                ppparent => ppparent%parent
             end if
          end do
          K(i+2,1) = GLUON
          K(i+2,2) = ppparent%type
          K(i+2,3) = i
          P(i+2,1) = ppparent%momentum%p(1)
          P(i+2,2) = ppparent%momentum%p(2)
          P(i+2,3) = ppparent%momentum%p(3)
          P(i+2,4) = ppparent%momentum%p(0)
          P(I+2,5) = ppparent%momentum**2
          !!! add partons in the initial state of the ME
          K(i+4,1) = GLUON
          K(i+4,2) = pp%type
          K(i+4,3) = i
          P(i+4,1) = pp%momentum%p(1)
          P(i+4,2) = pp%momentum%p(2)
          P(i+4,3) = pp%momentum%p(3)
          P(i+4,4) = pp%momentum%p(0)
          P(I+4,5) = pp%momentum**2
       else
          !!! for e+e- without ISR all entries are the same
          K(i,1) = GLUON
          K(i,2) = pp%type
          K(i,3) = 0
          P(i,1) = pp%momentum%p(1)
          P(i,2) = pp%momentum%p(2)
          P(i,3) = pp%momentum%p(3)
          P(i,4) = pp%momentum%p(0)
          P(I,5) = pp%momentum**2
          do j = 1, 5
             P(i+2,j) = P(1,j)
             K(i+2,j) = K(1,j)
             K(i+2,3) = i
             P(i+4,j) = P(1,j)
             K(i+4,j) = K(1,j)
             K(i+4,3) = i
          end do
          P(i+4,5) = 0.
       end if
    end do
    N = 6
    !!! create intermediate (fake) Z-Boson
    K(7,1) = GLUON
    K(7,2) = 23
    K(7,3) = 0
    P(7,1) = P(5,1) + P(6,1)
    P(7,2) = P(5,2) + P(6,2)
    P(7,3) = P(5,3) + P(6,3)
    P(7,4) = P(5,4) + P(6,4)
    P(7,5) = P(7,4)**2 - P(7,3)**2 - P(7,2)**2 - P(7,1)**2
    N = 7
    !!! include partons in the final state of the hard matrix element
    do i = 1, size (shower%interactions(1)%i%partons) - 2
       !!! get partons that are in the final state of the hard matrix element
       pp => shower%interactions(1)%i%partons(2+i)%p
       !!! add these partons to the event record
       K(7+I,1) = GLUON
       K(7+I,2) = pp%type
       K(7+I,3) = 7
       P(7+I,1) = pp%momentum%p(1)
       P(7+I,2) = pp%momentum%p(2)
       P(7+I,3) = pp%momentum%p(3)
       P(7+I,4) = pp%momentum%p(0)
       P(7+I,5) = P(7+I,4)**2 - P(7+I,3)**2 - P(7+I,2)**2 - P(7+I,1)**2
       N = 7 + I
    end do
    !!! include "Z" (again)
    N = N + 1
    K(N,1) = 11
    K(N,2) = 23
    K(N,3) = 7
    P(N,1) = P(7,1)
    P(N,2) = P(7,2)
    P(N,3) = P(7,3)
    P(N,4) = P(7,4)
    P(N,5) = P(7,5)
    nz = N
    !!! include partons from the final state of the parton shower
    call shower_transfer_final_partons_to_pythia (shower, 8)
    !!! set "children" of "Z"
    K(nz,4) = 11
    K(nz,5) = N
    !!! mark spacers
    MSTU(73) = N
    MSTU(74) = N

    !!! be sure to remove the next partons (=first obsolete partons)
    K(N+1,1) = 0
    K(N+1,2) = 0
    K(N+1,3) = 0
    K(N+2,1) = 0
    K(N+2,2) = 0
    K(N+2,3) = 0
    K(N+3,1) = 0
    K(N+3,2) = 0
    K(N+3,3) = 0
    !!! otherwise they might be interpreted as thrust information
  end subroutine shower_converttopythia

  subroutine shower_transfer_final_partons_to_pythia (shower, first)
    IMPLICIT DOUBLE PRECISION(A-H, O-Z)
    IMPLICIT INTEGER(I-N)
    !    C...  Commonblocks.
    COMMON/PYJETS/N,NPAD,K(4000,5),P(4000,5),V(4000,5)
    COMMON/PYDAT1/MSTU(200),PARU(200),MSTJ(200),PARJ(200)
    COMMON/PYDAT2/KCHG(500,4),PMAS(500,4),PARF(2000),VCKM(4,4)
    COMMON/PYDAT3/MDCY(500,3),MDME(8000,2),BRAT(8000),KFDP(8000,5)
    COMMON/PYSUBS/MSEL,MSELPD,MSUB(500),KFIN(2,-40:40),CKIN(200)
    COMMON/PYPARS/MSTP(200),PARP(200),MSTI(200),PARI(200)
    SAVE /PYJETS/,/PYDAT1/,/PYDAT2/,/PYDAT3/,/PYSUBS/,/PYPARS/

    type(shower_t), intent(in) :: shower
    integer, intent(in) :: first
    type(parton_t), pointer :: prt
    integer :: i, j, n_finals
    type(parton_t), dimension(:), allocatable :: final_partons
    type(parton_t) :: temp_parton
    integer :: minindex, maxindex

    prt => null()

    !!! get total number of final partons
    n_finals = 0
    do i = 1, size (shower%partons)
       if (.not. associated (shower%partons(i)%p)) cycle
       prt => shower%partons(i)%p
       if (.not. prt%belongstoFSR) cycle
       if (associated (prt%child1)) cycle
       n_finals = n_finals + 1
    end do

    allocate (final_partons(1:n_finals))
    j = 1
    do i = 1, size (shower%partons)
       if (.not. associated (shower%partons(i)%p)) cycle
       prt => shower%partons(i)%p
       if (.not. prt%belongstoFSR) cycle
       if (associated (prt%child1)) cycle
       final_partons(j) = shower%partons(i)%p
       j = j + 1
    end do

    !!! move quark to front as beginning of color string
    minindex = 1
    maxindex = size (final_partons)
    FIND_Q: do i = minindex, maxindex
       if (final_partons(i)%type >= 1 .and. final_partons(i)%type <= 6) then
          temp_parton = final_partons(minindex)
          final_partons(minindex) = final_partons(i)
          final_partons(i) = temp_parton
          exit FIND_Q
       end if
    end do FIND_Q

    !!! sort so that connected partons are next to each other, don't care about zeros
    do i = 1, size (final_partons)
       !!! ensure that final_partnons begins with a color (not an anticolor)
       if (final_partons(i)%c1 > 0 .and. final_partons(i)%c2 == 0) then
          if (i == 1) then
             exit
          else
             temp_parton = final_partons(1)
             final_partons(1) = final_partons(i)
             final_partons(i) = temp_parton
             exit
          end if
       end if
    end do

    do i = 1, size (final_partons) - 1
       !!! search for color partner and move it to i + 1
       PARTNERS: do j = i + 1, size (final_partons)
          if (final_partons(j)%c2 == final_partons(i)%c1) exit PARTNERS
       end do PARTNERS
       if (j > size (final_partons)) then
          print *, "no color connected parton found" !WRONG???
          print *, "particle: ", final_partons(i)%nr, " index: ", &
               final_partons(i)%c1
          exit
       end if
       temp_parton = final_partons(i + 1)
       final_partons(i + 1) = final_partons(j)
       final_partons(j) = temp_parton
    end do

    !!! transfering partons
    do i = 1, size (final_partons)
       prt = final_partons(i)
       N = N + 1
       K(N,1) = 2
       if (prt%c1 == 0) K(N,1) = 1       !!! end of color string
       K(N,2) = prt%type
       K(N,3) = first
       K(N,4) = 0
       K(N,5) = 0
       P(N,1) = prt%momentum%p(1)
       P(N,2) = prt%momentum%p(2)
       P(N,3) = prt%momentum%p(3)
       P(N,4) = prt%momentum%p(0)
       P(N,5) = prt%momentum**2
    end do
    deallocate (final_partons)
  end subroutine shower_transfer_final_partons_to_pythia


  end module shower_topythia

