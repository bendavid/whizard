! $Id: parameters.QED.omega.f90,v 1.1 2004/03/11 04:21:17 kilian Exp $
!
! Copyright (C) 1999-2009 by 
!     Wolfgang Kilian <kilian@hep.physik.uni-siegen.de>
!     Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
!     Juergen Reuter <juergen.reuter@physik.uni-freiburg.de>
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

module parameters_sm_top_anom
  use kinds
  use constants

  implicit none
  private

  real(default), dimension(27), public :: mass, width
  real(default), public :: as
  complex(default), public :: gs, igs, ig, unit

  real(default), public :: e, g, e_em
  real(default), public :: sinthw, costhw, sin2thw, tanthw
  real(default), public :: qelep, qeup, qedwn
  real(default), public :: ttop, tbot, tch, ttau, tw
  real(default), public :: ltop, lbot, lc, ltau, lw
  complex(default), public :: qlep, qup, qdwn, gcc, qw, &
       gzww, gwww, ghww, ghhww, ghzz, ghhzz, &
       ghbb, ghtt, ghcc, ghtautau, gh3, gh4, ghmm, & 		
     !  ghgaga, ghgaz, ghgg, ghmm, & 
       iqw, igzww, igwww, gw4, gzzww, gazww, gaaww
  real(default), public :: vev, lambda, scal
  complex(default), dimension(2), public :: &
       gncneu, gnclep, gncup, gncdwn, &
       tvaa, vlrz, tvaz, vlrw, tlrw, tvag
  integer, public :: fun_flag

  public :: init_parameters, model_update_alpha_s, &
       gmom, gtva_tta, gvlr_ttz, gtva_ttz, gvlr_btw, gvlr_tbw, &
       gtlr_btw, gtrl_tbw, gtlr_btwz, gtrl_tbwz, gtlr_btwa, gtrl_tbwa, &
       gtva_ttww, gtva_bbww, gtva_ttg, gtva_ttgg

  real(default), parameter :: &
          GF = 1.16639E-5_default   ! Fermi constant
  !!! This corresponds to 1/alpha = 137.03598949333
  real(default), parameter :: &
          alpha = 1.0_default/137.03598949333_default
  complex(default), parameter :: &
          alphas =  0.1178_default  ! Strong coupling constant (Z point)

contains

  subroutine init_parameters

    tvaa(1)     = 1
    tvaa(2)     = 1 * (0,1)
    vlrz(1)     = 0
    vlrz(2)     = 0
    tvaz(1)     = 1
    tvaz(2)     = 1 * (0,1)
    vlrw(1)     = 1 + 1 * (0,1)
    vlrw(2)     = 1 + 1 * (0,1)
    tlrw(1)     = 1 + 1 * (0,1)
    tlrw(2)     = 1 + 1 * (0,1)
    tvag(1)     = 1
    tvag(2)     = 1 * (0,1)
    lambda      = 1000
    fun_flag    = 0

    mass(1:27)  = 0
    width(1:27) = 0
    mass(3)     = 0.095_default        ! s-quark mass 
    mass(4)     = 1.2_default          ! c-quark mass
    ! mass(5)     = 4.2_default          ! b-quark mass
    ! mass(6)     = 173.1_default        ! t-quark mass
    ! width(6)    = 1.523_default        ! t-quark width
    mass(11)    = 0.000510997_default  ! electron mass
    mass(13)    = 0.105658389_default  ! muon mass
    mass(15)    = 1.77705_default      ! tau-lepton mas
    ! mass(23)    = 91.1882_default      ! Z-boson mass
    ! width(23)   = 2.443_default        ! Z-boson width
    ! mass(24)    = 80.419_default       ! W-boson mass
    ! width(24)   = 2.049_default        ! W-boson width
    mass(25)    = 200._default          ! Higgs mass
    width(25)   = 1.419_default        ! Higgs width

    ttop = 4.0_default * mass(6)**2 / mass(25)**2
    tbot = 4.0_default * mass(5)**2 / mass(25)**2
    tch  = 4.0_default * mass(4)**2 / mass(25)**2
    ttau = 4.0_default * mass(15)**2 / mass(25)**2
    tw   = 4.0_default * mass(24)**2 / mass(25)**2  
    ! ltop = 4.0_default * mass(6)**2 / mass(23)**2
    ! lbot = 4.0_default * mass(5)**2 / mass(23)**2  
    ! lc   = 4.0_default * mass(4)**2 / mass(23)**2
    ! ltau = 4.0_default * mass(15)**2 / mass(23)**2
    ! lw   = 4.0_default * mass(24)**2 / mass(23)**2

    e_em = sqrt(4.0_default * PI * alpha)
    vev = 1 / sqrt (sqrt (2.0_default) * GF)  ! v (Higgs vev)
    ! costhw = mass(24) / mass(23)  ! cos(theta-W)
    costhw = 0.881901_default
    sinthw = sqrt (1.0_default-costhw**2)  ! sin(theta-W)
    sin2thw = sinthw**2
    tanthw = sinthw/costhw
    ! e = 2.0_default * sinthw * mass(24) / vev  ! em-coupling (GF scheme)
    e = 0.349196_default
    qelep = - 1
    qeup = 2.0_default / 3.0_default
    qedwn = - 1.0_default / 3.0_default
    g = e / sinthw
    ig = cmplx (0.0_default, 1.0_default, kind=default) * g
    gcc = - g / 2 / sqrt (2.0_default)
    gncneu(1) = - g / 2 / costhw * ( + 0.5_default)
    gnclep(1) = - g / 2 / costhw * ( - 0.5_default - 2 * qelep * sin2thw)
    gncup(1)  = - g / 2 / costhw * ( + 0.5_default - 2 * qeup  * sin2thw)
    gncdwn(1) = - g / 2 / costhw * ( - 0.5_default - 2 * qedwn * sin2thw)
    gncneu(2) = - g / 2 / costhw * ( + 0.5_default)
    gnclep(2) = - g / 2 / costhw * ( - 0.5_default)
    ! gncup(2)  = - g / 2 / costhw * ( + 0.5_default)
    gncup(2)  = 0
    ! gncdwn(2) = - g / 2 / costhw * ( - 0.5_default)
    gncdwn(2) = 0
    qlep = - e * qelep
    qup = - e * qeup
    qdwn = - e * qedwn
    qw = e
    iqw = (0,1)*qw
    gzww = g * costhw
    igzww = (0,1)*gzww
    gwww = g
    igwww = (0,1)*gwww
    gw4 = gwww**2
    gzzww = gzww**2
    gazww = gzww * qw
    gaaww = qw**2
    ghww = mass(24) * g
    ghhww = g**2 / 2.0_default
    ghzz = mass(23) * g / costhw
    ghhzz = g**2 / 2.0_default / costhw**2
    ghtt = - mass(6) / vev
    ghbb = - mass(5) / vev
    ghcc = - mass(4) / vev
    ghtautau = - mass(15) / vev
    ghmm = - mass(13) / vev
    gh3 = - 3 * mass(25)**2 / vev
    gh4 = - 3 * mass(25)**2 / vev**2
    !!! Color flow basis, divide by sqrt(2)
    gs = sqrt(2.0_default*PI*alphas)
    igs = cmplx (0.0_default, 1.0_default, kind=default) * gs

    unit = 1.0_default

  end subroutine init_parameters

  subroutine model_update_alpha_s (alpha_s)
    real(default), intent(in) :: alpha_s
    gs = sqrt(2.0_default*PI*alpha_s)
    igs = cmplx (0.0_default, 1.0_default, kind=default) * gs     
  end subroutine model_update_alpha_s

  pure function gmom(k2, i, coeff, lam) result (c)
    complex(default) :: c
    real(default), intent(in) :: k2
    integer, intent(in) :: i
    complex(default), dimension(2), intent(in) :: coeff
    real(default), intent(in) :: lam
    select case (fun_flag)
      case (0)
        c = coeff(i) * vev / lam**2
      case (1)
        c = coeff(i) * vev / (lam**2 + k2)
      case (2)
        c = coeff(i) * vev * lam**2 / (lam**2 + k2)**2
      case (3)
        c = coeff(i) * vev * exp(-k2/lam**2) / lam**2
    end select
  end function gmom

  pure function gtva_tta(k2, i) result (c)
    complex(default) :: c
    real(default), intent(in) :: k2
    integer, intent(in) :: i
    c = - e * gmom(k2, i, tvaa, lambda)
  end function gtva_tta

  pure function gvlr_ttz(k2, i) result (c)
    complex(default) :: c
    real(default), intent(in) :: k2
    integer, intent(in) :: i
!    c = - mass(23) * gmom(k2, i, vlrz, lambda)
    c = - g * vev / (2.0_default*costhw) * gmom(k2, i, vlrz, lambda)
  end function gvlr_ttz

  pure function gtva_ttz(k2, i) result (c)
    complex(default) :: c
    real(default), intent(in) :: k2
    integer, intent(in) :: i
    c = - gmom(k2, i, tvaz, lambda)
  end function gtva_ttz

  pure function gvlr_btw(k2, i) result (c)
    complex(default) :: c
    real(default), intent(in) :: k2
    integer, intent(in) :: i
!    c = - sqrt(2.0_default)*mass(24)*gmom(k2, i, vlrw, lambda)
    c = - g * vev / sqrt(2.0_default) * gmom(k2, i, vlrw, lambda)
  end function gvlr_btw

  pure function gvlr_tbw(k2, i) result (c)
    complex(default) :: c
    real(default), intent(in) :: k2
    integer, intent(in) :: i
!    c = - sqrt(2.0_default)*mass(24)*gmom(k2, i, conjg(vlrw), lambda)
    c = - g * vev / sqrt(2.0_default) * gmom(k2, i, conjg(vlrw), lambda)
  end function gvlr_tbw

  pure function gtlr_btw(k2, i) result (c)
    complex(default) :: c
    real(default), intent(in) :: k2
    integer, intent(in) :: i
    c = - sqrt(2.0_default) * gmom(k2, i, tlrw, lambda)
  end function gtlr_btw

  pure function gtrl_tbw(k2, i) result (c)
    complex(default) :: c
    real(default), intent(in) :: k2
    integer, intent(in) :: i
    c = - sqrt(2.0_default) * gmom(k2, i, conjg(tlrw), lambda)
  end function gtrl_tbw

  pure function gtlr_btwa(k2, i) result (c)
    complex(default) :: c
    real(default), intent(in) :: k2
    integer, intent(in) :: i
    c = - sinthw * gtlr_btw(k2, i)
!    c = 0
  end function gtlr_btwa

  pure function gtrl_tbwa(k2, i) result (c)
    complex(default) :: c
    real(default), intent(in) :: k2
    integer, intent(in) :: i
    c = - sinthw * gtrl_tbw(k2, i)
!    c = 0
  end function gtrl_tbwa

  pure function gtlr_btwz(k2, i) result (c)
    complex(default) :: c
    real(default), intent(in) :: k2
    integer, intent(in) :: i
    c = - costhw * gtlr_btw(k2, i)
!    c = 0
  end function gtlr_btwz

  pure function gtrl_tbwz(k2, i) result (c)
    complex(default) :: c
    real(default), intent(in) :: k2
    integer, intent(in) :: i
    c = - costhw * gtrl_tbw(k2, i)
!    c = 0
  end function gtrl_tbwz

  pure function gtva_ttww(k2, i) result (c)
    complex(default) :: c
    real(default), intent(in) :: k2
    integer, intent(in) :: i
    complex(default) :: norm
    norm = - 1.0_default / (2.0_default * sqrt(2.0_default))
!    norm = 0
    select case (i)
      case (1)
        c = norm * real(gtlr_btw(k2, 2))
      case (2)
        c = norm * aimag(gtlr_btw(k2, 2)) * (0, 1)
    end select
  end function gtva_ttww

  pure function gtva_bbww(k2, i) result (c)
    complex(default) :: c
    real(default), intent(in) :: k2
    integer, intent(in) :: i
    complex(default) :: norm
    norm = - 1.0_default / (2.0_default * sqrt(2.0_default))
!    norm = 0
    select case (i)
      case (1)
        c = norm * real(gtlr_btw(k2, 1))
      case (2)
        c = - norm * aimag(gtlr_btw(k2, 1)) * (0, 1)
    end select
  end function gtva_bbww

  pure function gtva_ttg(k2, i) result (c)
    complex(default) :: c
    real(default), intent(in) :: k2
    integer, intent(in) :: i
    c = - gs * gmom(k2, i, tvag, lambda)
!    c = 0
  end function gtva_ttg

  pure function gtva_ttgg(k2, i) result (c)
    complex(default) :: c
    real(default), intent(in) :: k2
    integer, intent(in) :: i
!    c = - (0,1) * gs * gtva_ttg(k2, i)
    c = - gtva_ttg(k2, i)
!    c = 0
  end function gtva_ttgg

end module parameters_sm_top_anom
