! circe2.f90 -- beam spectra for linear colliders and photon colliders
! $Id: circe2.nw,v 1.56 2002/10/14 10:12:06 ohl Exp $
! Copyright (C) 2001-2012 by 
!      Wolfgang Kilian <kilian@physik.uni-siegen.de>
!      Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
!      Juergen Reuter <juergen.reuter@desy.de>
!      Christian Speckner <christian.speckner@physik.uni-freiburg.de>
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
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
module circe2
  use kinds

  implicit none
  private

  public :: circe2_params_t 
  public :: cir2gn
  public :: cir2ch
  public :: cir2gp
  public :: cir2lm
  public :: cir2dn
  public :: cir2dm
  public :: cir2ld

  integer, parameter :: NBMAX = 100, NCMAX = 36
  integer, parameter :: POLAVG = 1, POLHEL = 2, POLGEN = 3
    integer, parameter :: NBMMAX = 1

  type :: circe2_params_t
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
        integer, dimension(NCMAX) :: nb1, nb2
        logical, dimension(NCMAX) :: triang
        integer :: nc
        integer, dimension(NCMAX) :: pid1, pid2
        integer, dimension(NCMAX) :: pol1, pol2
        integer, dimension(NBMAX,NCMAX) :: map1, map2
        integer :: polspt
  end type circe2_params_t


  type(circe2_params_t), public, save :: c2p

contains

  subroutine cir2gn (p1, h1, p2, h2, y1, y2, rng)
    integer :: p1, h1, p2, h2
    real(kind=double) :: y1, y2
    external rng
    integer :: i, ic, i1, i2, ibot, itop
    real(kind=double) :: x1, x2
    real(kind=double) :: u, tmp
        ic = 0
        if (((c2p%polspt .eq. POLAVG) .or. (c2p%polspt .eq. POLGEN)) &
             .and. ((h1 .ne. 0) .or. (h2 .ne. 0))) then
           write (*, '(2A)') 'circe2: current beam description ', &
                'supports only polarization averages'
        else if ((c2p%polspt .eq. POLHEL) &
             .and. ((h1 .eq. 0) .or. (h2 .eq. 0))) then
           write (*, '(2A)') 'circe2: polarization averages ', &
                'not supported by current beam description'
        else
           do i = 1, c2p%nc
              if (       (p1 .eq. c2p%pid1(i)) .and. (h1 .eq. c2p%pol1(i)) & 
                   .and. (p2 .eq. c2p%pid2(i)) .and. (h2 .eq. c2p%pol2(i))) then
                 ic = i
              end if
           end do                                                              
        end if
        if (ic .le. 0) then
           write (*, '(A,2I4,A,2I3)') &
                'circe2: no channel for particles', p1, p2, &
                ' and polarizations', h1, h2
           y1 = -3.4E+38
           y2 = -3.4E+38
           return
        end if
    call rng (u)
          ibot = 0
          itop = c2p%nb1(ic) * c2p%nb2(ic)
          do 
            if (itop .le. (ibot + 1)) then
               i = ibot + 1
               exit
            else
               i = (ibot + itop) / 2
               if (u .lt. c2p%wgt(i,ic)) then
                  itop = i
               else
                  ibot = i
               end if
            end if 
          end do
          i2 = 1 + (i - 1) / c2p%nb1(ic)
          i1 = i - (i2 - 1) * c2p%nb1(ic)
          call rng (u)
          x1 = c2p%xb1(i1,ic)*u + c2p%xb1(i1-1,ic)*(1-u)
          call rng (u)
          x2 = c2p%xb2(i2,ic)*u + c2p%xb2(i2-1,ic)*(1-u)
        if (c2p%map1(i1,ic) .eq. 0) then
           y1 = x1
        else if (c2p%map1(i1,ic) .eq. 1) then
           y1 = (c2p%a1(i1,ic)*(x1-c2p%xi1(i1,ic)))**c2p%alpha1(i1,ic) / c2p%b1(i1,ic) &
                  + c2p%eta1(i1,ic)
        else if (c2p%map1(i1,ic) .eq. 2) then
           y1 = c2p%a1(i1,ic) * tan(c2p%a1(i1,ic)*(x1-c2p%xi1(i1,ic))/c2p%b1(i1,ic)**2) &
                  + c2p%eta1(i1,ic)
        else
           write (*, '(A,I3)') &
                'circe2: internal error: invalid map: ', c2p%map1(i1,ic)
        end if
        if (c2p%map2(i2,ic) .eq. 0) then
           y2 = x2
        else if (c2p%map2(i2,ic) .eq. 1) then
           y2 = (c2p%a2(i2,ic)*(x2-c2p%xi2(i2,ic)))**c2p%alpha2(i2,ic) / c2p%b2(i2,ic) &
                  + c2p%eta2(i2,ic)
        else if (c2p%map2(i2,ic) .eq. 2) then
           y2 = c2p%a2(i2,ic) * tan(c2p%a2(i2,ic)*(x2-c2p%xi2(i2,ic))/c2p%b2(i2,ic)**2) &
                  + c2p%eta2(i2,ic)
        else
           write (*, '(A,I3)') & 
                'circe2: internal error: invalid map: ', c2p%map2(i2,ic)
        end if
          if (c2p%triang(ic)) then
             y2 = y1 * y2
                   call rng (u)
                   if (2*u .ge. 1) then
                      tmp = y1
                      y1 = y2
                      y2 = tmp
                   end if
          end if
  end subroutine cir2gn

  subroutine cir2ch (p1, h1, p2, h2, rng)
    integer :: p1, h1, p2, h2
    external rng
    integer :: ic, ibot, itop
    real(kind=double) :: u
    call rng (u)
    ibot = 0
    itop = c2p%nc
    do
      if (itop .le. (ibot + 1)) then
         ic = ibot + 1
         p1 = c2p%pid1(ic)
         h1 = c2p%pol1(ic)
         p2 = c2p%pid2(ic)
         h2 = c2p%pol2(ic)
         exit
      else
         ic = (ibot + itop) / 2
         if (u .lt. c2p%cwgt(ic)) then
            itop = ic
         else
            ibot = ic
         end if
      end if
    end do
    write (*, '(A)') 'circe2: internal error'
    stop
  end subroutine cir2ch

  subroutine cir2gp (p1, p2, x1, x2, pol, rng)
    integer :: p1, p2
    real(kind=double) :: x1, x2
    real(kind=double), dimension(0:3,0:3) :: pol
    external rng
    integer :: h1, h2, i1, i2
    real(kind=double) :: pol00
    call cir2ch (p1, h1, p2, h2, rng)
    call cir2gn (p1, h1, p2, h2, x1, x2, rng)
    call cir2dm (p1, p2, x1, x2, pol)
    pol00 = pol(0,0)
    do i1 = 0, 4
       do i2 = 0, 4
          pol(i1,i2) = pol(i1,i2) / pol00
         end do     
    end do
  end subroutine cir2gp

  function cir2lm (p1, h1, p2, h2)
    integer :: p1, h1, p2, h2
    integer :: ic
    real(kind=double) :: cir2lm
    cir2lm = 0
    do ic = 1, c2p%nc
       if (       ((p1 .eq. c2p%pid1(ic)) .or. (p1 .eq. 0)) &
            .and. ((h1 .eq. c2p%pol1(ic)) .or. (h1 .eq. 0)) &
            .and. ((p2 .eq. c2p%pid2(ic)) .or. (p2 .eq. 0)) &
            .and. ((h2 .eq. c2p%pol2(ic)) .or. (h2 .eq. 0))) then
          cir2lm = cir2lm + c2p%lumi(ic)
       end if
    end do
  end function cir2lm

  function cir2dn (p1, h1, p2, h2, yy1, yy2)
    integer :: p1, h1, p2, h2
    real(kind=double) :: yy1, yy2
    real(kind=double) :: y1, y2
    real(kind=double) :: cir2dn
    integer :: i, ic, i1, i2, ibot, itop
        ic = 0
        if (((c2p%polspt .eq. POLAVG) .or. (c2p%polspt .eq. POLGEN)) &
             .and. ((h1 .ne. 0) .or. (h2 .ne. 0))) then
           write (*, '(2A)') 'circe2: current beam description ', &
                'supports only polarization averages'
        else if ((c2p%polspt .eq. POLHEL) &
             .and. ((h1 .eq. 0) .or. (h2 .eq. 0))) then
           write (*, '(2A)') 'circe2: polarization averages ', &
                'not supported by current beam description'
        else
           do i = 1, c2p%nc
              if (       (p1 .eq. c2p%pid1(i)) .and. (h1 .eq. c2p%pol1(i)) & 
                   .and. (p2 .eq. c2p%pid2(i)) .and. (h2 .eq. c2p%pol2(i))) then
                 ic = i
              end if
           end do                                                              
        end if
    if (ic .le. 0) then
       cir2dn = 0
       return
    end if
          if (c2p%triang(ic)) then
             y1 = max (yy1, yy2)
             y2 = min (yy1, yy2) / y1
          else
             y1 = yy1
             y2 = yy2
          end if
    if (     (y1 .lt. c2p%yb1(0,ic)) .or. (y1 .gt. c2p%yb1(c2p%nb1(ic),ic)) &
        .or. (y2 .lt. c2p%yb2(0,ic)) .or. (y2 .gt. c2p%yb2(c2p%nb2(ic),ic))) then
       cir2dn = 0
       return
    end if
          ibot = 0
          itop = c2p%nb1(ic)
          do
            if (itop .le. (ibot + 1)) then
               i1 = ibot + 1
               exit
            else
               i1 = (ibot + itop) / 2
               if (y1 .lt. c2p%yb1(i1,ic)) then
                  itop = i1
               else
                  ibot = i1
               end if
            end if
          end do      
          ibot = 0
          itop = c2p%nb2(ic)
          do
            if (itop .le. (ibot + 1)) then
               i2 = ibot + 1
            else
               i2 = (ibot + itop) / 2
               if (y2 .lt. c2p%yb2(i2,ic)) then
                  itop = i2
               else
                  ibot = i2
               end if
            end if
          end do
    cir2dn = c2p%val(i1,i2,ic)
        if (c2p%map1(i1,ic) .eq. 0) then
        else if (c2p%map1(i1,ic) .eq. 1) then
           cir2dn = cir2dn * c2p%b1(i1,ic) / (c2p%a1(i1,ic)*c2p%alpha1(i1,ic)) &
                * (c2p%b1(i1,ic)*(y1-c2p%eta1(i1,ic)))**(1/c2p%alpha1(i1,ic)-1)
        else if (c2p%map1(i1,ic) .eq. 2) then
           cir2dn = cir2dn * c2p%b1(i1,ic)**2 &
                / ((y1-c2p%eta1(i1,ic))**2 + c2p%a1(i1,ic)**2)
        else
           write (*, '(A,I3)') &
                'circe2: internal error: invalid map: ', c2p%map1(i1,ic)
           stop
        end if
        if (c2p%map2(i2,ic) .eq. 0) then
        else if (c2p%map2(i2,ic) .eq. 1) then
           cir2dn = cir2dn * c2p%b2(i2,ic) / (c2p%a2(i2,ic)*c2p%alpha2(i2,ic)) &
                * (c2p%b2(i2,ic)*(y2-c2p%eta2(i2,ic)))**(1/c2p%alpha2(i2,ic)-1)
        else if (c2p%map2(i2,ic) .eq. 2) then
           cir2dn = cir2dn * c2p%b2(i2,ic)**2 &
                / ((y2-c2p%eta2(i2,ic))**2 + c2p%a2(i2,ic)**2)
        else
           write (*, '(A,I3)') &
                'circe2: internal error: invalid map: ', c2p%map2(i2,ic)
           stop
        end if
          if (c2p%triang(ic)) then
             cir2dn = cir2dn / y1
          end if
  end function cir2dn

  subroutine cir2dm (p1, p2, x1, x2, pol)
    integer :: p1, p2
    real(kind=double) :: x1, x2
    real(kind=double) :: pol(0:3,0:3)
        if (c2p%polspt .ne. POLGEN) then
           write (*, '(2A)') 'circe2: current beam ', &
                'description supports no density matrices'
           return
        end if
    print *, 'circe2: cir2dm not implemented yet!'
  end subroutine cir2dm

  subroutine cir2ld (file, design, roots, ierror)
    character(len=*) :: file, design
    real(kind=double) :: roots
    integer :: ierror
    character(len=72) :: buffer
    character(len=72) :: fdesgn
    character(len=72) :: fpolsp
    real(kind=double) :: froots
    integer :: lun, loaded, prefix
    logical :: match
        integer :: i, ic
        integer :: i1, i2
        real(kind=double) :: w
        integer :: status
        logical :: exists, isopen
        integer, parameter :: EOK = 0
        integer, parameter :: EFILE = -1
        integer, parameter :: EMATCH = -2
        integer, parameter :: EFORMT = -3
        integer, parameter :: ESIZE = -4
        do lun = 10, 99
           inquire (unit = lun, exist = exists, &
                   opened = isopen, iostat = status)
           if ((status .eq. 0) .and. exists .and. .not.isopen) then
             goto 11
           end if
        end do
        write (*, '(A)') 'cir2ld: no free unit'
        ierror = ESIZE
        stop
    11 continue
    loaded = 0
          open (unit = lun, file = file, status = 'old', iostat = status)
          if (status .ne. 0) then
             write (*, '(2A)') 'cir2ld: can''t open ', file
             ierror = EFILE
             return
          end if
    if (ierror .gt. 0) then
       write (*, '(2A)') 'cir2ld: $Id: circe2.nw,v 1.56 2002/10/14 10:12:06 ohl Exp $'
    end if
    prefix = index (design, '*') - 1
100 continue
        20   continue
                read (lun, '(A)', end = 29) buffer
                if (buffer(1:6) .eq. 'CIRCE2') then
                   goto 21
                else if (buffer(1:1) .eq. '!') then
                   if (ierror .gt. 0) then
                      write (*, '(A)') buffer
                   end if
                   goto 20
                end if
                write (*, '(A)') 'cir2ld: invalid file'
                ierror = EFORMT
                return
        29   continue
             if (loaded .gt. 0) then           
                close (unit = lun)
                ierror = EOK
             else
                ierror = EMATCH
             end if
             return
        21   continue
       if (buffer(8:15) .eq. 'FORMAT#1') then
          read (lun, *)
          read (lun, *) fdesgn, froots
              match = .false.
              if (fdesgn .eq. design) then
                 match = .true.
              else if (prefix .eq. 0) then
                 match = .true.
              else if (prefix .gt. 0) then
                 if (fdesgn(1:min(prefix,len(fdesgn))) &
                      .eq. design(1:min(prefix,len(design)))) then
                    match = .true.
                 end if
              end if
          if (match .and. (abs (froots - roots) .le. 1d0)) then
                 read (lun, *) 
                 read (lun, *) c2p%nc, fpolsp
                 if (c2p%nc .gt. NCMAX) then
                    write (*, '(A)') 'cir2ld: too many channels'
                    ierror = ESIZE
                    return
                 end if
                     if (      (fpolsp(1:1).eq.'a') &
                          .or. (fpolsp(1:1).eq.'A')) then
                        c2p%polspt = POLAVG
                     else if (      (fpolsp(1:1).eq.'h') &
                               .or. (fpolsp(1:1).eq.'H')) then
                        c2p%polspt = POLHEL
                     else if (      (fpolsp(1:1).eq.'d') &
                               .or. (fpolsp(1:1).eq.'D')) then
                        c2p%polspt = POLGEN
                     else
                        write (*, '(A,I5)') &
                             'cir2ld: invalid polarization support: ', fpolsp
                        ierror = EFORMT
                        return
                     end if
                 c2p%cwgt(0) = 0
                 do ic = 1, c2p%nc
                        read (lun, *)
                        read (lun, *) c2p%pid1(ic), c2p%pol1(ic), &
                              c2p%pid2(ic), c2p%pol2(ic), c2p%lumi(ic)
                        c2p%cwgt(ic) = c2p%cwgt(ic-1) + c2p%lumi(ic)
                            if (c2p%polspt .eq. POLAVG &
                                 .and. (      (c2p%pol1(ic) .ne. 0)  &
                                         .or. (c2p%pol2(ic) .ne. 0))) then
                               write (*, '(A)') 'cir2ld: expecting averaged polarization'
                               ierror = EFORMT
                               return
                            else if (c2p%polspt .eq. POLHEL &
                                 .and. (      (c2p%pol1(ic) .eq. 0)       &
                                         .or. (c2p%pol2(ic) .eq. 0))) then
                               write (*, '(A)') 'cir2ld: expecting helicities'
                               ierror = EFORMT
                               return
                            else if (c2p%polspt .eq. POLGEN) then
                               write (*, '(A)') 'cir2ld: general polarizations not supported yet'
                               ierror = EFORMT
                               return
                            else if (c2p%polspt .eq. POLGEN &
                                 .and. (      (c2p%pol1(ic) .ne. 0)       &
                                         .or. (c2p%pol2(ic) .ne. 0))) then
                               write (*, '(A)') 'cir2ld: expecting pol = 0'
                               ierror = EFORMT
                               return
                            end if
                        read (lun, *)
                        read (lun, *) c2p%nb1(ic), c2p%nb2(ic), c2p%triang(ic)
                        if ((c2p%nb1(ic) .gt. NBMAX) .or. (c2p%nb2(ic) .gt. NBMAX)) then
                           write (*, '(A)') 'cir2ld: too many bins'
                           ierror = ESIZE
                           return
                        end if
                        read (lun, *)
                        read (lun, *) c2p%xb1(0,ic)
                        do i1 = 1, c2p%nb1(ic)
                           read (lun, *) c2p%xb1(i1,ic), c2p%map1(i1,ic), c2p%alpha1(i1,ic), &
                               c2p%xi1(i1,ic), c2p%eta1(i1,ic), c2p%a1(i1,ic), c2p%b1(i1,ic)
                        end do
                        read (lun, *)
                        read (lun, *) c2p%xb2(0,ic)
                        do i2 = 1, c2p%nb2(ic)
                           read (lun, *) c2p%xb2(i2,ic), c2p%map2(i2,ic), c2p%alpha2(i2,ic), &
                               c2p%xi2(i2,ic), c2p%eta2(i2,ic), c2p%a2(i2,ic), c2p%b2(i2,ic)
                        end do
                        do i = 0, c2p%nb1(ic)
                           i1 = max (i, 1)
                           if (c2p%map1(i1,ic) .eq. 0) then
                              c2p%yb1(i,ic) = c2p%xb1(i,ic)
                           else if (c2p%map1(i1,ic) .eq. 1) then
                              c2p%yb1(i,ic) = &
                                   (c2p%a1(i1,ic) &
                                     * (c2p%xb1(i,ic)-c2p%xi1(i1,ic)))**c2p%alpha1(i1,ic) &
                                        / c2p%b1(i1,ic) + c2p%eta1(i1,ic)
                           else if (c2p%map1(i1,ic) .eq. 2) then
                              c2p%yb1(i,ic) = c2p%a1(i1,ic) &
                                   * tan(c2p%a1(i1,ic)/c2p%b1(i1,ic)**2 &
                                          * (c2p%xb1(i,ic)-c2p%xi1(i1,ic))) &
                                       + c2p%eta1(i1,ic)
                           else
                              write (*, '(A,I3)') 'cir2ld: invalid map: ', c2p%map1(i1,ic)
                              ierror = EFORMT
                              return
                           end if
                        end do
                        do i = 0, c2p%nb2(ic)
                           i2 = max (i, 1)
                           if (c2p%map2(i2,ic) .eq. 0) then
                              c2p%yb2(i,ic) = c2p%xb2(i,ic)
                           else if (c2p%map2(i2,ic) .eq. 1) then
                              c2p%yb2(i,ic) &
                                   = (c2p%a2(i2,ic) &
                                       * (c2p%xb2(i,ic)-c2p%xi2(i2,ic)))**c2p%alpha2(i2,ic) &
                                          / c2p%b2(i2,ic) + c2p%eta2(i2,ic)
                           else if (c2p%map2(i2,ic) .eq. 2) then
                              c2p%yb2(i,ic) = c2p%a2(i2,ic) &
                                   * tan(c2p%a2(i2,ic)/c2p%b2(i2,ic)**2 &
                                          * (c2p%xb2(i,ic)-c2p%xi2(i2,ic))) &
                                       + c2p%eta2(i2,ic)
                           else
                              write (*, '(A,I3)') 'cir2ld: invalid map: ', c2p%map2(i2,ic)
                              ierror = EFORMT
                              return
                           end if
                        end do
                        read (lun, *)
                        c2p%wgt(0,ic) = 0
                        do i = 1, c2p%nb1(ic)*c2p%nb2(ic)
                           read (lun, *) w
                           c2p%wgt(i,ic) = c2p%wgt(i-1,ic) + w
                                 i2 = 1 + (i - 1) / c2p%nb1(ic)
                                 i1 = i - (i2 - 1) * c2p%nb1(ic)
                           c2p%val(i1,i2,ic) = w & 
                                / (  (c2p%xb1(i1,ic) - c2p%xb1(i1-1,ic)) &
                                   * (c2p%xb2(i2,ic) - c2p%xb2(i2-1,ic)))
                        end do
                        c2p%wgt(c2p%nb1(ic)*c2p%nb2(ic),ic) = 1
                 end do   
                 do ic = 1, c2p%nc
                    c2p%cwgt(ic) = c2p%cwgt(ic) / c2p%cwgt(c2p%nc)
                 end do
             loaded = loaded + 1
          else
              101  continue
                   read (lun, *) buffer
                   if (buffer(1:6) .ne. 'ECRIC2') then
                      goto 101
                   end if
             goto 100
          end if
       else
          write (*, '(2A)') 'cir2ld: invalid format: ', buffer(8:72)
          ierror = EFORMT
          return
       end if
             read (lun, '(A)') buffer
             if (buffer(1:6) .ne. 'ECRIC2') then
                write (*, '(A)') 'cir2ld: invalid file'
                ierror = EFORMT
                return
             end if
       goto 100
  end subroutine cir2ld
  
end module circe2
