! $Id: parameters.SM_tt_threshold.f90,v 1.4 2006/06/16 13:31:48 kilian Exp $
!
! Copyright (C) 1999-2012 by 
!     Wolfgang Kilian <kilian@physik.uni-siegen.de>
!     Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
!     Juergen Reuter <juergen.reuter@desy.de>
!     Christian Speckner <christian.speckner@physik.uni-freiburg.de>
!     Fabian Bach <fabian.bach@desy.de> (only this file)
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
module parameters_sm_tt_threshold
  use kinds
  use constants 
  use sm_physics !NODEP!
  use omega_vectors
  use ttv_formfactors
  implicit none
  private

  real(default), dimension(27), public :: mass, width
  real(default), public :: as
  complex(default), public :: gs, igs

  real(default), public :: e, g, e_em
  real(default), public :: sinthw, costhw, sin2thw, tanthw
  real(default), public :: qelep, qeup, qedwn
  complex(default), public :: qlep, qup, qdwn, gcc, qw, &
       gzww, gwww, ghww, ghhww, ghzz, ghhzz, &
       ghbb, ghtt, ghcc, ghtautau, gh3, gh4, ghmm, & 
       iqw, igzww, igwww, gw4, gzzww, gazww, gaaww
  real(default), public :: vev
  complex(default), dimension(2), public :: &
       gncneu, gnclep, gncup, gncdwn

  public :: import_from_whizard, model_update_alpha_s, &
       va_ilc_tta, va_ilc_ttz, ttv_mtpole

contains

  subroutine import_from_whizard (par_array)
    real(default), dimension(30), intent(in) :: par_array
    type :: parameter_set
       real(default) :: mZ
       real(default) :: mW
       real(default) :: mH
       real(default) :: alphas
       real(default) :: me
       real(default) :: mmu
       real(default) :: mtau
       real(default) :: ms
       real(default) :: mc
       real(default) :: mb
       real(default) :: wZ
       real(default) :: wW
       real(default) :: wH
       real(default) :: khgaz
       real(default) :: khgaga
       real(default) :: khgg
       real(default) :: xi0
       real(default) :: xipm
       real(default) :: alphaemi
       real(default) :: m1s
       real(default) :: wtop
       real(default) :: nloop
       real(default) :: sh
       real(default) :: sf
       real(default) :: test
       real(default) :: ee
       real(default) :: cw
       real(default) :: sw
       real(default) :: v
       real(default) :: mtpole
    end type parameter_set
    type(parameter_set) :: par
    !!! This corresponds to 1/alpha = 137.03598949333
    real(default), parameter :: &
         alpha = 1.0_default/137.03598949333_default
    e_em = sqrt(4.0_default * PI * alpha)
    par%mZ     = par_array(1)
    par%mW     = par_array(2)
    par%mH     = par_array(3)
    par%alphas = par_array(4)
    par%me     = par_array(5)
    par%mmu    = par_array(6)
    par%mtau   = par_array(7)
    par%ms     = par_array(8)
    par%mc     = par_array(9)
    par%mb     = par_array(10)
    par%wZ     = par_array(11)
    par%wW     = par_array(12)
    par%wH     = par_array(13)
    par%khgaz  = par_array(14)
    par%khgaga = par_array(15)
    par%khgg   = par_array(16)
    par%xi0    = par_array(17)
    par%xipm   = par_array(18)
    par%alphaemi = par_array(19)
    par%m1s    = par_array(20)
    par%wtop   = par_array(21)
    par%nloop  = par_array(22)
    par%sh     = par_array(23)
    par%sf     = par_array(24)
    par%test   = par_array(25)
    par%ee     = par_array(26)
    par%cw     = par_array(27)
    par%sw     = par_array(28)
    par%v      = par_array(29)
    par%mtpole = par_array(30)
    mass(1:27) = 0
    width(1:27) = 0
    mass(3) = par%ms
    mass(4) = par%mc
    mass(5) = par%mb
    mass(6) = par%mtpole
    width(6) = par%wtop
    mass(11) = par%me
    mass(13) = par%mmu
    mass(15) = par%mtau
    mass(23) = par%mZ
    width(23) = par%wZ
    mass(24) = par%mW
    width(24) = par%wW
    mass(25) = par%mH
    width(25) = par%wH
    mass(26) =  par%xi0 * mass(23)
    width(26) =  0
    mass(27) =  par%xipm * mass(24)
    width(27) =  0
    vev = par%v
    e = par%ee
    sinthw = par%sw
    sin2thw = par%sw**2
    costhw = par%cw
    tanthw = sinthw/costhw
    qelep = - 1
    qeup = 2.0_default / 3.0_default
    qedwn = - 1.0_default / 3.0_default
    g = e / sinthw
    gcc = - g / 2 / sqrt (2.0_default)
    gncneu(1) = - g / 2 / costhw * ( + 0.5_default)
    gnclep(1) = - g / 2 / costhw * ( - 0.5_default - 2 * qelep * sin2thw)
    gncup(1)  = - g / 2 / costhw * ( + 0.5_default - 2 * qeup  * sin2thw)
    gncdwn(1) = - g / 2 / costhw * ( - 0.5_default - 2 * qedwn * sin2thw)
    gncneu(2) = - g / 2 / costhw * ( + 0.5_default)
    gnclep(2) = - g / 2 / costhw * ( - 0.5_default)
    gncup(2)  = - g / 2 / costhw * ( + 0.5_default)
    gncdwn(2) = - g / 2 / costhw * ( - 0.5_default)
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
    gs = sqrt(2.0_default*PI*par%alphas)
    igs = cmplx (0.0_default, 1.0_default, kind=default) * gs
    call ttv_formfactors_init (mass(6), par%m1s, width(6), par%alphaemi, &
       par%sw, par%alphas, par%mZ, par%mW, mass(5), par%sh, par%sf, par%nloop)
    call ttv_formfactors_init_threshold_grid (par%test)
  end subroutine import_from_whizard

  subroutine model_update_alpha_s (alpha_s)
    real(default), intent(in) :: alpha_s
    gs = sqrt(2.0_default*PI*alpha_s)
    igs = cmplx (0.0_default, 1.0_default, kind=default) * gs     
  end subroutine model_update_alpha_s

  pure function ttv_formfactor (p, k, i) result (c)
    complex(default) :: c
    type(momentum), intent(in) :: p, k
    integer, intent(in) :: i
    real(default) :: p2, k2, q2
    p2 = p*p
    k2 = k*k
    q2 = (k+p)*(k+p)
    !!! lim_(alphas->0) FF = 0
    c = ttv_formfactors_FF_threshold (p2, k2, q2, i)
  end function ttv_formfactor

  pure function va_ilc_tta (p, k, i) result (c)
    complex(default) :: c
    type(momentum), intent(in) :: p, k
    integer, intent(in) :: i
    c = 0.0_default
    if ( i==1 ) c = qup * ttv_formfactor (p, k, 1)
  end function va_ilc_tta

  pure function va_ilc_ttz (p, k, i) result (c)
    complex(default) :: c
    type(momentum), intent(in) :: p, k
    integer, intent(in) :: i
    c = gncup(i) * ttv_formfactor (p, k, i)
  end function va_ilc_ttz

  pure function ttv_mtpole (s) result (m)
    real(default), intent(in) :: s
    real(default) :: m
    m = ttv_formfactors_m1s_to_mpole ( sqrt(s) )
  end function ttv_mtpole
end module parameters_sm_tt_threshold
