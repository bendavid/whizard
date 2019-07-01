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
  use diagnostics
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
  complex(default), public :: &
       gccq11 = 0, gccq12 = 0, gccq13 = 0, gccq21 = 0, &
       gccq22 = 0, gccq23 = 0, gccq31 = 0, gccq32 = 0, gccq33 = 0
  real(default), public :: vev
  complex(default), dimension(2), public :: &
       gncneu, gnclep, gncup, gncdwn

  integer, public :: FF

  public :: import_from_whizard, model_update_alpha_s, &
       ttv_formfactor, va_ilc_tta, va_ilc_ttz, ttv_mtpole, &
       onshell_tops, expanded_amp2

contains

  subroutine import_from_whizard (par_array)
    real(default), dimension(40), intent(in) :: par_array
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
       real(default) :: Vtb
       real(default) :: wt_inv
       real(default) :: nloop
       real(default) :: sh
       real(default) :: sf
       real(default) :: FF
       real(default) :: v1
       real(default) :: v2
       real(default) :: scan_sqrts_min
       real(default) :: scan_sqrts_max
       real(default) :: scan_sqrts_stepsize
       real(default) :: test
       real(default) :: no_pwave
       real(default) :: mpole_fixed
       real(default) :: ee
       real(default) :: cw
       real(default) :: sw
       real(default) :: v
       real(default) :: mtpole
       real(default) :: wtop
    end type parameter_set
    type(parameter_set) :: par
    !!! This corresponds to 1/alpha = 137.03598949333
    real(default), parameter :: &
         alpha = 1.0_default/137.03598949333_default
    logical :: no_pwave, mpole_fixed
    call msg_debug (D_THRESHOLD, "import_from_whizard")
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
    par%Vtb    = par_array(21)
    par%wt_inv = par_array(22)
    par%nloop  = par_array(23)
    par%sh     = par_array(24)
    par%sf     = par_array(25)
    par%FF     = par_array(26)
    par%v1     = par_array(27)
    par%v2     = par_array(28)
    par%scan_sqrts_min = par_array(29)
    par%scan_sqrts_max = par_array(30)
    par%scan_sqrts_stepsize = par_array(31)
    par%test   = par_array(32)
    par%no_pwave = par_array(33)
    par%mpole_fixed = par_array(34)
    par%ee     = par_array(35)
    par%cw     = par_array(36)
    par%sw     = par_array(37)
    par%v      = par_array(38)
    par%mtpole = par_array(39)
    par%wtop   = par_array(40)
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
    mass(26) = par%xi0 * mass(23)
    width(26) = 0
    mass(27) = par%xipm * mass(24)
    width(27) = 0
    FF = par%FF
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
    gccq11 = gcc * 1.0_default
    gccq12 = 0.0_default
    gccq13 = 0.0_default
    gccq21 = 0.0_default
    gccq22 = gcc * 1.0_default
    gccq23 = 0.0_default
    gccq31 = 0.0_default
    gccq32 = 0.0_default
    gccq33 = gcc * par%Vtb
    gncneu(1) = - g / 2 / costhw * ( + 0.5_default)
    gnclep(1) = - g / 2 / costhw * ( - 0.5_default - 2 * qelep * sin2thw)
    gncup(1)  = - g / 2 / costhw * ( + 0.5_default - 2 * qeup  * sin2thw)
    gncdwn(1) = - g / 2 / costhw * ( - 0.5_default - 2 * qedwn * sin2thw)
    gncneu(2) = - g / 2 / costhw * ( + 0.5_default)
    gnclep(2) = - g / 2 / costhw * ( - 0.5_default)
    no_pwave = par%no_pwave > 0.0_default
    if (no_pwave) then
       gncup(2) = 0.0_default
    else
       gncup(2)  = - g / 2 / costhw * ( + 0.5_default)
    end if
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
    mpole_fixed = par%mpole_fixed > 0.0_default
    call init_parameters &
         (mass(6), width(6), par%m1s, par%Vtb, par%wt_inv, &
          par%alphaemi, par%sw, par%alphas, par%mZ, par%mW, &
          mass(5), par%sh, par%sf, par%nloop, par%FF, &
          par%v1, par%v2, par%scan_sqrts_min, par%scan_sqrts_max, &
          par%scan_sqrts_stepsize, mpole_fixed)
    call init_threshold_grids (par%test)
  end subroutine import_from_whizard

  subroutine model_update_alpha_s (alpha_s)
    real(default), intent(in) :: alpha_s
    gs = sqrt(2.0_default*PI*alpha_s)
    igs = cmplx (0.0_default, 1.0_default, kind=default) * gs
  end subroutine model_update_alpha_s

  !pure
  function ttv_formfactor (p, k, i, FF_mode) result (c)
    complex(default) :: c
    type(momentum), intent(in) :: p, k
    integer, intent(in) :: i
    integer, intent(in), optional :: FF_mode
    type(phase_space_point_t) :: ps
    integer :: this_FF
    call ps%init (p*p, k*k, (k+p)*(k+p), mass(6))
    this_FF = FF; if (present (FF_mode))  this_FF = FF_mode
    c = FF_master (ps, i, this_FF)
    !!! form factors include tree level: FF = 1 + O(alphas)
    !!! subtract tree level contribution ~ 1 already included in SM couplings
    c = c - 1.0_default
  end function ttv_formfactor

  !pure
  function onshell_tops (p, k) result (onshell)
    logical :: onshell
    type(momentum), intent(in) :: p, k
    type(phase_space_point_t) :: ps
    call ps%init (p*p, k*k, (k+p)*(k+p), mass(6))
    onshell = ps%onshell
  end function onshell_tops

  !pure
  function va_ilc_tta (p, k, i) result (c)
    complex(default) :: c
    type(momentum), intent(in) :: p, k
    integer, intent(in) :: i
    c = 0.0_default
    if (i==1) c = qup * ttv_formfactor (p, k, 1)
  end function va_ilc_tta

  !pure
  function va_ilc_ttz (p, k, i) result (c)
    complex(default) :: c
    type(momentum), intent(in) :: p, k
    integer, intent(in) :: i
    c = gncup(i) * ttv_formfactor (p, k, i)
  end function va_ilc_ttz

  !pure
  function ttv_mtpole (s) result (m)
    real(default), intent(in) :: s
    real(default) :: m
    m = m1s_to_mpole (sqrt (s))
  end function ttv_mtpole

  !pure
  function expanded_amp2 (amp_tree, amp_blob) result (amp2)
    real(default) :: amp2
    complex(default), dimension(:), intent(in) :: amp_tree, amp_blob
    amp2 = sum (amp_tree * conjg (amp_tree) + &
         amp_tree * conjg (amp_blob) + &
         amp_blob * conjg (amp_tree))
  end function expanded_amp2
end module parameters_sm_tt_threshold
