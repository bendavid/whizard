!!! module: shower_topythia_module
!!! This code is part of my Ph.D studies.
!!! 
!!! Copyright (C) 2011 Sebastian Schmidt <sebastian.t.schmidt@desy.de>
!!! 
!!! This program is free software; you can redistribute it and/or modify it
!!! under the terms of the GNU General Public License as published by the Free 
!!! Software Foundation; either version 3 of the License, or (at your option) 
!!! any later version.
!!! 
!!! This program is distributed in the hope that it will be useful, but WITHOUT
!!! ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or 
!!! FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for
!!! more details.
!!! 
!!! You should have received a copy of the GNU General Public License along
!!! with this program; if not, see <http://www.gnu.org/licenses/>.
!!! 
!!! Latest Change: Thu Jan 13 17:21:30 2011 Time zone: 3600 seconds
!!! 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
module shower_topythia_module

  USE kinds, ONLY: default !NODEP!
  use shower_basics_module
  use shower_parton_module
  use shower_module

  IMPLICIT NONE

  public :: shower_converttopythia

  contains
    
    SUBROUTINE shower_converttopythia(shower)
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

      TYPE(shower_t), INTENT(in) :: shower
      TYPE(parton_t), POINTER :: pp, ppparent
      integer :: i, j, nz

      ! currently only works for one interaction

      DO i=1, 2
         ! get history of the event
         pp=>shower%interactions(1)%i%partons(i)%p
         ! add these partons to the event record
         if(associated(pp%initial)) then
            ! add hadrons
            K(i,1)=21
            K(i,2)=pp%initial%typ
            K(i,3)=0
            P(i,1)=vector4_get_component(pp%initial%momentum,1)
            P(i,2)=vector4_get_component(pp%initial%momentum,2)
            P(i,3)=vector4_get_component(pp%initial%momentum,3)
            P(i,4)=vector4_get_component(pp%initial%momentum,0)
            P(I,5)=pp%initial%momentum**2
            ! add partons emitted by the hadron
            ppparent => pp
            do while(associated(ppparent%parent))
               if(parton_is_hadron(ppparent%parent)) then
                  exit
               else
                  ppparent => ppparent%parent
               end if
            end do
            K(i+2,1)=21
            K(i+2,2)=ppparent%typ
            K(i+2,3)=i
            P(i+2,1)=vector4_get_component(ppparent%momentum,1)
            P(i+2,2)=vector4_get_component(ppparent%momentum,2)
            P(i+2,3)=vector4_get_component(ppparent%momentum,3)
            P(i+2,4)=vector4_get_component(ppparent%momentum,0)
            P(I+2,5)=ppparent%momentum**2
            ! add partons in the initial state of the ME
            K(i+4,1)=21
            K(i+4,2)=pp%typ
            K(i+4,3)=i
            P(i+4,1)=vector4_get_component(pp%momentum,1)
            P(i+4,2)=vector4_get_component(pp%momentum,2)
            P(i+4,3)=vector4_get_component(pp%momentum,3)
            P(i+4,4)=vector4_get_component(pp%momentum,0)
            P(I+4,5)=pp%momentum**2
         else
            ! for e+e- without ISR all entries are the same
            K(i,1)=21
            K(i,2)=pp%typ
            K(i,3)=0
            P(i,1)=vector4_get_component(pp%momentum,1)
            P(i,2)=vector4_get_component(pp%momentum,2)
            P(i,3)=vector4_get_component(pp%momentum,3)
            P(i,4)=vector4_get_component(pp%momentum,0)
            P(I,5)=pp%momentum**2
            DO j=1,5
               P(i+2,j)=P(1,j)
               K(i+2,j)=K(1,j)
               K(i+2,3)=i
               P(i+4,j)=P(1,j)
               K(i+4,j)=K(1,j)
               K(i+4,3)=i
            END DO
            P(i+4,5)=0.
         end if
      ENDDO
      N=6
      ! create intermediate (fake) Z-Boson
      K(7,1)=21
      K(7,2)=23
      K(7,3)=0
      P(7,1)=P(5,1)+P(6,1)
      P(7,2)=P(5,2)+P(6,2)
      P(7,3)=P(5,3)+P(6,3)
      P(7,4)=P(5,4)+P(6,4)
      P(7,5)=P(7,4)**2-P(7,3)**2-P(7,2)**2-P(7,1)**2
      N=7
      ! include partons in the final state of the hard matrix element
      DO i=1, size(shower%interactions(1)%i%partons)-2
         ! get partons that are in the final state of the hard matrix element
         pp => shower%interactions(1)%i%partons(2+i)%p
         ! add these partons to the event record
         K(7+I,1)=21
         K(7+I,2)=pp%typ
         K(7+I,3)=7
         P(7+I,1)=vector4_get_component(pp%momentum, 1)
         P(7+I,2)=vector4_get_component(pp%momentum, 2)
         P(7+I,3)=vector4_get_component(pp%momentum, 3)
         P(7+I,4)=vector4_get_component(pp%momentum, 0)
         P(7+I,5)=P(7+I,4)**2-P(7+I,3)**2-P(7+I,2)**2-P(7+I,1)**2
         N=7+I
      ENDDO
      ! include "Z" (again)
      N=N+1
      K(N,1)=11
      K(N,2)=23
      K(N,3)=7
      P(N,1)=P(7,1)
      P(N,2)=P(7,2)
      P(N,3)=P(7,3)
      P(N,4)=P(7,4)
      P(N,5)=P(7,5)
      nz = N
      ! include partons from the final state of the parton shower
      call shower_transfer_final_partons_to_pythia(shower, 8)
      ! set "children" of "Z"
      K(nz,4)=11
      K(nz,5)=N
      ! mark spacers
      MSTU(73)=N
      MSTU(74)=N

      ! be sure to remove the next partons (=first obsolete partons)
      K(N+1,1)=0
      K(N+1,2)=0
      K(N+1,3)=0
      K(N+2,1)=0
      K(N+2,2)=0
      K(N+2,3)=0
      K(N+3,1)=0
      K(N+3,2)=0
      K(N+3,3)=0
      ! otherwise they might be interpreted as thrust information
    END SUBROUTINE shower_converttopythia

    subroutine shower_transfer_final_partons_to_pythia(shower, first)
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

!!$      !! old version using (anti-)colorpartner pointers
!!$
!!$      partons: do i=1, size(shower%partons)
!!$         ! loop over partons to find quarks = beginnings of color strings
!!$         if(.not. associated(shower%partons(i)%p)) cycle
!!$         prt=> shower%partons(i)%p
!!$         if(associated(prt%child1)) cycle
!!$         if(.not. parton_is_quark(prt)) cycle
!!$         if(.not. prt%typ > 0 ) cycle
!!$
!!$         color_string: do
!!$            ! transfer prt to PYHIA
!!$            N=N+1
!!$            K(N,1)=2
!!$            K(N,2)=prt%typ
!!$            K(N,3)=first
!!$            K(N,4)=0
!!$            K(N,5)=0
!!$            P(N,1)=vector4_get_component(prt%momentum, 1)
!!$            P(N,2)=vector4_get_component(prt%momentum, 2)
!!$            P(N,3)=vector4_get_component(prt%momentum, 3)
!!$            P(N,4)=vector4_get_component(prt%momentum, 0)
!!$            P(N,5)=prt%t
!!$            
!!$            if(associated(prt%colorpartner)) then
!!$               prt=>prt%colorpartner
!!$               cycle color_string
!!$            else
!!$               K(N,1) = 1 !mark end of string
!!$               exit color_string
!!$            end if
!!$         end do color_string
!!$      end do partons
!!$
      !! new version using color indices

      ! get total number of final partons
      n_finals=0
      do i=1, size(shower%partons)
         if(.not. associated(shower%partons(i)%p)) cycle
         prt=> shower%partons(i)%p
         if(prt%belongstoFSR .eqv. .false.) cycle
         if(associated(prt%child1)) cycle
         n_finals = n_finals + 1
      end do
      print *, "n_finals=", n_finals

      allocate(final_partons(1:n_finals))
      j=1
      do i=1, size(shower%partons)
         if(.not. associated(shower%partons(i)%p)) cycle
         prt=> shower%partons(i)%p
         if(prt%belongstoFSR .eqv. .false.) cycle
         if(associated(prt%child1)) cycle
         final_partons(j) = shower%partons(i)%p
         j = j+1
      end do
      
      do i=1, size(final_partons)
         call parton_print(final_partons(i))
      end do

      !! move quark to front as beginning of color string
      minindex=1
      maxindex=size(final_partons)
      find_q: do i=minindex, maxindex
         if(final_partons(i)%typ .ge. 1 .and. final_partons(i)%typ .le. 6) then
            temp_parton = final_partons(minindex)
            final_partons(minindex) = final_partons(i)
            final_partons(i) = temp_parton
            exit find_q
         end if
      end do find_q

      ! sort so that connected partons are next to each other, don't care about zeros
      do i=1, size(final_partons)
         ! ensure that final_partnons begins with a color (not an anticolor)
         if(final_partons(i)%c1>0 .and. final_partons(i)%c2.eq.0) then
            if(i.eq.1) then
               exit
            else
               temp_parton = final_partons(1)
               final_partons(1) = final_partons(i)
               final_partons(i) = temp_parton
               exit
            end if
         end if
      end do
      
      do i=1, size(final_partons)-1
         ! search for color partner and move it to i+1
         partners: do j=i+1, size(final_partons)
            if(final_partons(j)%c2 .eq. final_partons(i)%c1) exit partners
         end do partners
         if(j>size(final_partons)) then
            print *, "no color connected parton found" !WRONG???
            print *, "particle: ", final_partons(i)%nr, " index: ", final_partons(i)%c1
!            pause
            exit
         end if
         temp_parton = final_partons(i+1)
         final_partons(i+1) = final_partons(j)
         final_partons(j) = temp_parton
      end do
      
      do i=1, size(final_partons)
         call parton_print(final_partons(i))
      end do

      ! transfering partons
      do i=1, size(final_partons)
         prt=final_partons(i)
         N=N+1
         K(N,1)=2
         if(prt%c1.eq.0) K(N,1)=1       ! end of color string
         K(N,2)=prt%typ
         K(N,3)=first
         K(N,4)=0
         K(N,5)=0
         P(N,1)=vector4_get_component(prt%momentum, 1)
         P(N,2)=vector4_get_component(prt%momentum, 2)
         P(N,3)=vector4_get_component(prt%momentum, 3)
         P(N,4)=vector4_get_component(prt%momentum, 0)
         P(N,5)=prt%momentum**2
!!$         call shower_topythia_recursiv_weighted(final_partons(i),1,
      end do
      deallocate(final_partons)
    end subroutine shower_transfer_final_partons_to_pythia

    recursive subroutine shower_topythia_recursiv_weighted(prt, mode, first)
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

      TYPE(parton_t), INTENT(in), target :: prt
      INTEGER, INTENT(in) :: mode, first
      integer :: n_emissions
      type(parton_t), pointer :: tempprt, finalprt
      real(kind=default), dimension(:), allocatable :: costhetas
      type(parton_pointer_t), dimension(:), allocatable :: emittedpartons
      integer :: i, max
      real(kind=default) :: maxcostheta

      n_emissions = 0

      IF(parton_is_final(prt)) THEN
         N=N+1
         K(N,1)=2
         IF(parton_is_quark(prt)) THEN
            ! check if quark is end of a color connection
            IF(prt%typ<0) THEN
               K(N,1)=1
            END IF
         END IF
         K(N,2)=prt%typ
         K(N,3)=first
         K(N,4)=0
         K(N,5)=0
         P(N,1)=vector4_get_component(prt%momentum, 1)
         P(N,2)=vector4_get_component(prt%momentum, 2)
         P(N,3)=vector4_get_component(prt%momentum, 3)
         P(N,4)=vector4_get_component(prt%momentum, 0)
         P(N,5)=prt%t
      ELSE
         ! search for following final partons
         IF(parton_is_gluon(prt)) THEN
            IF(parton_is_gluon(prt%child1)) THEN
               ! g-> gg so sequence is unimportant
               CALL shower_topythia_recursiv_weighted(prt%child1,1,first)
               CALL shower_topythia_recursiv_weighted(prt%child2,1,first)
            ELSE
               ! g-> qqbar -> use antiquark first, so that color flow is given correctly
               if(prt%child1%typ<0) then
                  CALL shower_topythia_recursiv_weighted(prt%child1,1,first)
                  CALL shower_topythia_recursiv_weighted(prt%child2,2,first)
               else
                  CALL shower_topythia_recursiv_weighted(prt%child2,2,first)
                  CALL shower_topythia_recursiv_weighted(prt%child1,1,first)
               end if
            ENDIF
         ELSE
            ! parton is quark
            ! find the emitted gluons and order them by the emission angle
            n_emissions=0
            tempprt=prt
            do
               ! calculate how many emissions there are
               if(associated(tempprt%child1)) then
                  tempprt=>tempprt%child1
                  n_emissions = n_emissions +1
                  cycle
               else
                  exit
               end if
            end do
            
            allocate(costhetas(1:n_emissions))
            allocate(emittedpartons(1:n_emissions))

            tempprt=>prt
            n_emissions=1
            do
               if(associated(tempprt%child1)) then
                  costhetas(n_emissions) = parton_get_costheta_korrekt(tempprt)
                  emittedpartons(n_emissions)%p => tempprt%child2
                  n_emissions= n_emissions +1
                  tempprt=> tempprt%child1
                  cycle
               else
                  finalprt=>tempprt
                  exit
               end if
            end do
            
            ! if mode .eq. 1 write quark first
            if(mode.eq.1) call shower_topythia_recursiv_weighted(finalprt, 1, first)

            ! if mode .eq. 2 write out gluons in recursive order <= replace costheta by 1- costheta
            if(mode.eq.2) then
               do i=1, size(costhetas)
                  costhetas(i) = 1._default - costhetas(i)
               end do
            end if
            
            do
               max=0
               maxcostheta=0._default
               do i=1, size(costhetas)
                  if(costhetas(i)>maxcostheta) then
                     maxcostheta = costhetas(i)
                     max=i
                  end if
               end do
               if(maxcostheta .eq. 0._default) then
                  exit
               end if

               call shower_topythia_recursiv_weighted(emittedpartons(max)%p, mode, first)
               costhetas(max) = 0._default
            end do

            ! if mode .eq. 2 write quark last
            if(mode .eq. 2) then 
               call shower_topythia_recursiv_weighted(finalprt, 2, first)
            end if

!!$            IF(mode.EQ.1) THEN
!!$               CALL shower_topythia_recursiv_weighted(prt%child1,1,first)
!!$               CALL shower_topythia_recursiv_weighted(prt%child2,1,first)
!!$            ELSE
!!$               CALL shower_topythia_recursiv_weighted(prt%child2,2,first)
!!$               CALL shower_topythia_recursiv_weighted(prt%child1,2,first)
!!$            END IF
         END IF
      END IF
    end subroutine shower_topythia_recursiv_weighted

    RECURSIVE SUBROUTINE shower_topythia_recursiv(prt, mode,first)
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

      TYPE(parton_t), INTENT(in) :: prt
      INTEGER, INTENT(in) :: mode, first

      IF(parton_is_final(prt)) THEN
         N=N+1
         K(N,1)=2
         IF(parton_is_quark(prt)) THEN
            ! check if quark is end of a color connection
            IF(prt%typ<0) THEN
               K(N,1)=1
            END IF
         END IF
         K(N,2)=prt%typ
         K(N,3)=first
         K(N,4)=0
         K(N,5)=0
         P(N,1)=vector4_get_component(prt%momentum, 1)
         P(N,2)=vector4_get_component(prt%momentum, 2)
         P(N,3)=vector4_get_component(prt%momentum, 3)
         P(N,4)=vector4_get_component(prt%momentum, 0)
         P(N,5)=prt%t
      ELSE
         ! search for following final partons
         IF(parton_is_gluon(prt)) THEN
            IF(parton_is_gluon(prt%child1)) THEN
               ! g-> gg so sequence is unimportant
               CALL shower_topythia_recursiv(prt%child1,1,first)
               CALL shower_topythia_recursiv(prt%child2,1,first)
            ELSE
               ! g-> qqbar -> use antiquark first, so that color flow is given correctly
               if(prt%child1%typ<0) then
                  CALL shower_topythia_recursiv(prt%child1,1,first)
                  CALL shower_topythia_recursiv(prt%child2,2,first)
               else
                  CALL shower_topythia_recursiv(prt%child2,2,first)
                  CALL shower_topythia_recursiv(prt%child1,1,first)
               end if
            ENDIF
         ELSE
            IF(mode.EQ.1) THEN
               CALL shower_topythia_recursiv(prt%child1,1,first)
               CALL shower_topythia_recursiv(prt%child2,1,first)
            ELSE
               CALL shower_topythia_recursiv(prt%child2,2,first)
               CALL shower_topythia_recursiv(prt%child1,2,first)
            END IF
         END IF
      END IF
    END SUBROUTINE shower_topythia_recursiv

  end module shower_topythia_module
