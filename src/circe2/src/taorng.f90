! $Id: postlude.nw,v 1.24 2001/10/30 11:47:54 ohl Exp $
!   Copyright (C) 1996-2011 by 
!       Wolfgang Kilian <kilian@physik.uni-siegen.de>
!       Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
!       Juergen Reuter <juergen.reuter@desy.de>
!       Christian Speckner <christian.speckner@physik.uni-freiburg.de>
!
!   Taorng is free software; you can redistribute it and/or modify it
!   under the terms of the GNU General Public License as published by
!   the Free Software Foundation; either version 2, or (at your option)
!   any later version.
!
!   Taorng is distributed in the hope that it will be useful, but
!   WITHOUT ANY WARRANTY; without even the implied warranty of
!   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!   GNU General Public License for more details.
!
!   You should have received a copy of the GNU General Public License
!   along with this program; if not, write to the Free Software
!   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
module tao_rng
  use kinds

  implicit none     
  private

  public :: taorni
  public :: taornu
  public :: taornv                 
  public :: taornj
  public :: taornl
  public :: taorng
  public :: taorns
  public :: taornt  

contains

  subroutine taorni (r)
    integer :: r
        integer, parameter :: NN = 1000
        integer, dimension(NN), save :: a(NN)
        integer :: i, n
        !!! common /taornb/ a, i, n
        !!! save /taornb/
              integer MAGIC0
              parameter (MAGIC0 = 19950826)
        integer :: magic
        !!! common /taornb/ magic
        if (magic .ne. MAGIC0) then
           n = NN
           i = n
           magic = MAGIC0
        end if
        i = i + 1
        if (i .gt. n) then
           call taorng (a, NN)
           i = 1
        end if
    r = a(i) 
  end subroutine taorni

  subroutine taornu (r)
    real(kind=double) :: r
        integer , parameter :: M = 2**30
    real(kind=double), parameter :: INVM = 1D0/M
        integer, parameter :: NN = 1000
        integer, dimension(NN), save :: a(NN)
        integer :: i, n
        !!! common /taornb/ a, i, n
        !!! save /taornb/
              integer MAGIC0
              parameter (MAGIC0 = 19950826)
        integer :: magic
        !!! common /taornb/ magic
        if (magic .ne. MAGIC0) then
           n = NN
           i = n
           magic = MAGIC0
        end if
        i = i + 1
        if (i .gt. n) then
           call taorng (a, NN)
           i = 1
        end if
    r = INVM * a(i) 
  end subroutine taornu

  subroutine taornv (v, nu)
    real(kind=double), dimension(:) :: v
    integer :: nu
        integer , parameter :: M = 2**30
    real(kind=double), parameter :: INVM = 1D0/M
    integer :: done, todo, chunk, k
        integer, parameter :: NN = 1000
        integer, dimension(NN), save :: a(NN)
        integer :: i, n
        !!! common /taornb/ a, i, n
        !!! save /taornb/
              integer MAGIC0
              parameter (MAGIC0 = 19950826)
        integer :: magic
        !!! common /taornb/ magic
        if (magic .ne. MAGIC0) then
           n = NN
           i = n
           magic = MAGIC0
        end if
        if (i .ge. n) then
           call taorng (a, NN)
           i = 0
        endif
        done = 0
        todo = nu
        chunk = min (todo, n - i)
        do k = 1, chunk
           v(k) = INVM * a(i+k)
        end do
    do
            i = i + chunk
            done = done + chunk
            todo = todo - chunk
            chunk = min (todo, n)
      if (chunk .le. 0) then
         exit
      else
               call taorng (a, NN)
               i = 0
               do k = 1, chunk
                  v(done+k) = INVM * a(k)
               end do
      end if
    end do
  end subroutine taornv

  subroutine taornj (j, nu)
    integer, dimension(:) :: j
    integer :: nu
    integer :: done, todo, chunk, k
        integer, parameter :: NN = 1000
        integer, dimension(NN), save :: a(NN)
        integer :: i, n
        !!! common /taornb/ a, i, n
        !!! save /taornb/
              integer MAGIC0
              parameter (MAGIC0 = 19950826)
        integer :: magic
        !!! common /taornb/ magic
        if (magic .ne. MAGIC0) then
           n = NN
           i = n
           magic = MAGIC0
        end if
        if (i .ge. n) then
           call taorng (a, NN)
           i = 0
        endif
        done = 0
        todo = nu
        chunk = min (todo, n - i)
          do k = 1, chunk
             j(k) = a(i+k)
          end do
    do 
            i = i + chunk
            done = done + chunk
            todo = todo - chunk
            chunk = min (todo, n)
      if (chunk .le. 0) then
         exit
      else
               call taorng (a, NN)
               i = 0
               do k = 1, chunk
                  j(done+k) = a(k)
               end do
      end if
    end do
  end subroutine taornj

  subroutine taornl (luxury)
    integer :: luxury
        integer, parameter :: NN = 1000
        integer, dimension(NN), save :: a(NN)
        integer :: i, n
        !!! common /taornb/ a, i, n
        !!! save /taornb/
              integer MAGIC0
              parameter (MAGIC0 = 19950826)
        integer :: magic
        !!! common /taornb/ magic
        if (magic .ne. MAGIC0) then
           n = NN
           i = n
           magic = MAGIC0
        end if
        if (luxury .gt. NN) then
           print *, 'taornl: luxury ', luxury, ' too high!'
           print *, 'taornl: will use 1 random number out of ', NN, '!'
           n = 1
        else if (luxury .lt. 1) then
           print *, 'taornl: luxury ', luxury, ' invalid!'
           print *, 'taornl: will use every random number!'
           n = NN
        else
           n = NN / luxury
        end if
        i = min (i, n)
  end subroutine taornl
    
  subroutine taorng (a, n)
    integer :: n
    integer, dimension(n) :: a
          integer, parameter :: K = 100, L = 37
        integer , parameter :: M = 2**30
                integer MAGIC0
                parameter (MAGIC0 = 19950826)
          integer ranx(K), magic
          common /taornc/ ranx, magic
          save /taornc/
    integer :: j
        if (magic .ne. MAGIC0) call taorns (0)
          do 10 j = 1, K
             a(j) = ranx(j)
     10   continue
          do 11 j = K+1, n
             a(j) = a(j-K) - a(j-L)
             if (a(j) .lt. 0) a(j) = a(j) + M
     11   continue
          do 20 j = 1, L
             ranx(j) = a(n+j-K) - a(n+j-L)
             if (ranx(j) .lt. 0) ranx(j) = ranx(j) + M
     20   continue
          do 21 j = L+1, K
             ranx(j) = a(n+j-K) - ranx(j-L)
             if (ranx(j) .lt. 0) ranx(j) = ranx(j) + M
     21   continue
  end subroutine taorng

  subroutine taorns (seedin)
    integer, intent(in) :: seedin
    integer :: seed
          integer, parameter :: K = 100, L = 37
        integer , parameter :: M = 2**30
        integer, parameter :: SEEDMX = 2**30 - 3
        integer, parameter :: TT= 70, KK = K+K-1
                integer MAGIC0
                parameter (MAGIC0 = 19950826)
          integer ranx(K), magic
          common /taornc/ ranx, magic
          save /taornc/
        integer x(KK), j, s, t
        seed = seedin
        if ((seed .lt. 0) .or. (seed .gt. SEEDMX)) then
           print *, 'taorns: seed (', seed, ') not in [0,', SEEDMX, ']!'
           seed = mod (abs (seed), SEEDMX+1)
           print *, 'taorns: seed set to ', seed, '!'
        end if
        s = seed - mod (seed, 2) + 2
        do j = 1, K
           x(j) = s
           s = s + s
           if (s .ge. M) s = s - M + 2
        end do
        do j = K+1, KK
           x(j) = 0
        end do
        x(2) = x(2) + 1
        s = seed
        t = TT - 1
    do
               do j = K, 2, -1
                  x(j+j-1) = x(j)
               end do
               do j = KK, K-L+2, -2
                  x(KK-j+2) = x(j) - mod (x(j), 2)
               end do
               do j = KK, K+1, -1
                  if (mod (x(j), 2) .eq. 1) then
                     x(j-(K-L)) = x(j-(K-L)) - x(j)
                     if (x(j-(K-L)) .lt. 0) x(j-(K-L)) = x(j-(K-L)) + M
                     x(j-K) = x(j-K) - x(j)
                     if (x(j-K) .lt. 0) x(j-K) = x(j-K) + M
                  end if
               end do
               if (mod (s, 2) .eq. 1) then
                  do j = K, 1, -1
                     x(j+1) = x(j)
                  end do
                  x(1) = x(K+1)
                  if (mod (x(K+1), 2) .eq. 1) then
                     x(L+1) = x(L+1) - x(K+1)
                     if (x(L+1) .lt. 0) x(L+1) = x(L+1) + M
                  end if
               end if
               if (s .ne. 0) then
                  s = s / 2
               else
                  t = t - 1
               end if
        if (t .le. 0) exit
    end do 
          do j = 1, L
             ranx(j+K-L) = x(j)
          end do
          do j = L+1, K
             ranx(j-L) = x(j)
          end do
    magic = MAGIC0
  end subroutine taorns

  subroutine taornt ()
    implicit none
        integer, parameter :: NN = 1000
        integer, dimension(NN), save :: a(NN)
        integer :: i, n
        !!! common /taornb/ a, i, n
        !!! save /taornb/
              integer MAGIC0
              parameter (MAGIC0 = 19950826)
        integer :: magic
        !!! common /taornb/ magic
    integer :: j, r
    integer, dimension(10), save :: expect = &
      (/ 640345214,  443605255,  411993687, 618952382, 123106306, &
         949854402,  429877922,  261135009, 574783260, 1043288376 /)
    write (*, 100) 'testing taorng ...'
    write (*, 100) '  call taornl (luxury=1)'
100  format (1X, A)
    call taornl (1)
    write (*, 100) '  call taorns (seed=0)'
    call taorns (0)
          i = N
    print *, '  10000 warmup calls to taorni'
    do j = 1, 10000
       call taorni (r)
    end do
    do j = 1, 10
       call taorni (r)
       if (r .eq. expect(j)) then
          write (*, 101) 10000+j, r
101        format (3X, I5, ': ', I10, ' OK.')
        else
           write (*, 102) 10000+j, r, expect(j)
102        format (3X, I5, ': ', I10, ' not OK, (expected ', I10, ')!')
         end if
      end do
      write (*, 100) 'done.'
      stop
    end subroutine taornt

end module tao_rng
