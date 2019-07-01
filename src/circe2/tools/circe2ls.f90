! circe2ls.f90 -- beam spectra for linear colliders and photon colliders
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
  program circe2ls
    use kinds

      implicit none

    integer :: lun
    character(len=72) :: buffer
    character(len=72) :: file
    character(len=60) :: design, polspt
    integer :: pid1, hel1, pid2, hel2, nc
    real(kind=double) :: roots, lumi
    integer :: status
    logical :: exists, isopen
    integer :: ierror
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
    write (*, '(A)') 'enter name of Circe2 data file:'
    read (*, '(A)') buffer
    file = buffer
    open (unit = lun, file = file, status = 'old', iostat = status)
    if (status .ne. 0) then
       write (*, '(2A)') 'circe2: can''t open ', file
       stop
    end if
    write (*, '(A,1X,A)') 'file:', file
30   continue
    read (lun, '(A)', end = 39) buffer
    if (buffer(1:7) .eq. 'design,') then
       read (lun, *) design, roots
       read (lun, *)
       read (lun, *) nc, polspt
             write (*, '(2A)')     '        design: ', design
             write (*, '(A,F7.1)') '       sqrt(s): ', roots
             write (*, '(A,I3)')   '     #channels: ', nc
             write (*, '(2A)')     '  polarization: ', polspt
             write (*, '(4X,4(A5,2X),A)') &
              'pid#1', 'hel#1', 'pid#2', 'hel#2', &
              'luminosity / (10^32cm^-2sec^-1)'
    else if (buffer(1:5) .eq. 'pid1,') then
       read (lun, *) pid1, hel1, pid2, hel2, lumi
             write (*, '(4X,4(I5,2X),F10.2,1X)') pid1, hel1, pid2, hel2, lumi
    end if
    goto 30
39   continue
  end program circe2ls

