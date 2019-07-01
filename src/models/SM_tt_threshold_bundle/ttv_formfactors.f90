! WHIZARD <<Version>> <<Date>>

! Copyright (C) 1999-2015 by 
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

module ttv_formfactors
  use kinds
  use constants, only: imago, pi
  use physics_defs, only: CF, CA, TR
  use sm_physics
  use interpolation !NODEP!
  use nr_hypgeometric
  implicit none
  save
  private

  logical :: init = .false.
  logical :: nustar_dynamic, scan_threshold

  real(default) :: m1s, gam
  integer :: nloop = 1
  real(default) :: mtpole = 0.0_default
  real(default) :: mtpole_init = 0.0_default
  real(default) :: h, mu_h, ah
  real(default) :: f, mu_s, as, asLL, nustar_fixed
  real(default), parameter :: nustar_offset = 0.05_default
  real(default) :: mu_u, au
  real(default), parameter :: nf = 5.0_default
  real(default) :: a1, a2, b0, b1
  real(default), dimension(2) :: aa2, aa3, aa4, aa5, aa8, aa0, current_c, matching_c
  real(default), dimension(0:2) :: xc
  integer :: ff_init
  real(default) :: sqrts_ref = -1.e9

  integer, parameter :: n_pt = 360
  real(default), parameter :: sq_lo=-10.0_default, sq_hi=30.0_default, sq_it=0.1_default
  integer, parameter :: n_en = int((sq_hi-sq_lo)/sq_it)+1
  real(default), dimension(n_en) :: en_data, scale_data
  real(default), dimension(n_pt) :: pt_data
  complex(default), dimension(n_en,n_pt,2) :: ff_data
  logical, dimension(n_en,2) :: unstable

  interface ttv_formfactors_init
    module procedure init_parameters
  end interface ttv_formfactors_init
  public :: ttv_formfactors_init

  interface ttv_formfactors_init_threshold_grid
    module procedure init_threshold_grid
  end interface ttv_formfactors_init_threshold_grid
  public :: ttv_formfactors_init_threshold_grid

  interface ttv_formfactors_FF
    module procedure FF_master
  end interface ttv_formfactors_FF
  public :: ttv_formfactors_FF

  interface ttv_formfactors_FF_threshold
    module procedure FF_threshold
  end interface ttv_formfactors_FF_threshold
  public :: ttv_formfactors_FF_threshold

  interface ttv_formfactors_FF_continuum
    module procedure FF_continuum
  end interface ttv_formfactors_FF_continuum
  public :: ttv_formfactors_FF_continuum

  interface ttv_formfactors_FF_approx
    module procedure FF_approx
  end interface ttv_formfactors_FF_approx
  public :: ttv_formfactors_FF_approx

  interface ttv_formfactors_m1s_to_mpole
    module procedure m1s_to_mpole
  end interface ttv_formfactors_m1s_to_mpole
  public :: ttv_formfactors_m1s_to_mpole

contains

  subroutine init_parameters (mpole_out, m1s_in, gam_inout, aemi, sw, az, mz, mw, mb, &
                              h_in, f_in, nloop_in)
    real(default), intent(out) :: mpole_out
    real(default), intent(in) :: m1s_in
    real(default), intent(inout) :: gam_inout
    real(default), intent(in) :: aemi
    real(default), intent(in) :: sw
    real(default), intent(in) :: az
    real(default), intent(in) :: mz
    real(default), intent(in) :: mw
    real(default), intent(in) :: mb
    real(default), intent(in) :: h_in
    real(default), intent(in) :: f_in
    real(default), intent(in) :: nloop_in
    real(default) :: z3
    !!! possibly (re-)enable these as user parameters:
    real(default) :: nu_in = -1.0_default
    real(default) :: ff_in = 1.0_default
    real(default) :: Vtb = 1.0_default
    init = .false.
    m1s = m1s_in
    if ( int(nloop_in) > nloop ) then
      print *, "  WARNING: reset to highest available nloop = ", nloop
    else
      nloop = int(nloop_in)
    end if

    !!! handle the top width consistently: not smaller than open t->bW channel
    gam = compute_gamtot_LO (1./aemi, sw, Vtb, m1s, mw, mb)
    if (gam_inout < gam ) then
      print *, "WARNING: Total top width wtop = ", gam_inout
      print *, "         inconsistent with visible t->Wb channel !!!"
      print *, "         Smallest consistent width: gam_tWb = ", gam
    end if
    gam = gam_inout

    !!! global hard parameters incl. hard alphas used in *all* form factors
    h    = h_in
    mu_h = m1s * h
    ah   = running_as (mu_h, az, mz, 2, nf)
    !!! auxiliary numbers needed later
    z3 = 1.20205690315959428539973816151_default
    b0 = coeff_b0(nf) * (4.*pi)
    b1 = coeff_b1(nf) * (4.*pi)**2
    a1 = 31./9.*CA - 20./9.*TR*nf
    a2 = ((4343./162. + 4.*pi**2 - pi**4/4. + 22./3.*z3)*CA**2 - &
         (1798./81. + 56./3.*z3)*CA*TR*nf - &
         (55./3. - 16.*z3)*CF*TR*nf + &
         (20./9.*TR*nf)**2 )
    !!! hard s/p-wave 1-loop matching coefficients, cf. arXiv:hep-ph/0604072
    matching_c(1) = ( 1.0_default - 2.*(CF/pi) * ah )
    matching_c(2) = ( 1.0_default -    (CF/pi) * ah )
    !!! current coefficients Ai(S,L,J), cf. arXiv:hep-ph/0609151, Eqs. (63)-(64)
    !!! 3S1 coefficients (s-wave, vector current)
    aa2(1) = (CF*(CA*CF*(9.*CA - 100.*CF) - &
              b0*(26.*CA**2 + 19.*CA*CF - 32.*CF**2)))/(26.*b0**2 *CA)
    aa3(1) = CF**2/( b0**2 *(6.*b0 - 13.*CA)*(b0 - 2.*CA)) * &
              (CA**2 *(9.*CA - 100.*CF) + b0*CA*(74.*CF - CA*16.) - &
              6.*b0**2 *(2.*CF - CA))
    aa4(1) = (24.*CF**2 * (11.*CA - 3.*b0)*(5.*CA + 8.*CF)) / &
              (13.*CA*(6.*b0 - 13.*CA)**2)
    aa5(1) =  (CF**2 * (CA*(15.-28) + b0*5.))/(6.*(b0-2.*CA)**2)
    aa8(1) = 0.0_default
    aa0(1) = -((8.*CF*(CA + CF)*(CA + 2.*CF))/(3.*b0**2))
    !!! 3P1 coefficients (p-wave, axial vector current)
    aa2(2) = -1./3. * (CF*(CA+2.*CF)/b0 - CF**2/(4.*b0) )
    aa3(2) =  0.0_default
    aa4(2) =  0.0_default
    aa5(2) =  1./3. * CF**2/(4.*(b0-2.*CA))
    aa8(2) = -1./3. * CF**2/(b0-CA)
    aa0(2) = -1./3. * 8.*CA*CF*(CA+4.*CF)/(3.*b0**2)

    !!! soft parameters incl. mtpole (depend on sqrts: init with sqrts ~ 2*m1s)
    nustar_fixed = nu_in
    nustar_dynamic = ( nustar_fixed  < 0. )
    f = f_in
    call update_soft_parameters ( 2.*m1s )
    mtpole_init = mtpole
    mpole_out = mtpole_init
    ff_init = int(ff_in)
    scan_threshold = ( ff_init < 2 )

    init = .true.
  end subroutine init_parameters

  subroutine init_threshold_grid (testflag)
    real(default), intent(in) :: testflag
    logical :: test
    test = ( testflag > 0.0_default )
    if ( .not.init .or. .not.scan_threshold ) return
    print *, " "
    print *, "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"
    if (test) then
      print *, " TESTING ONLY:"
      print *, " Skip initialization of ttbar threshold resummation and use tree-level SM."
    else
      print *, " Initialize e+e- => ttbar threshold resummation:"
      print *, " Use analytic (LL) or TOPPIK (NLL) form factors for ttA/ttZ vector"
      print *, " and axial vector couplings (S/P-wave) in the threshold region."
      print *, " Cf. threshold shapes from A. Hoang et al.: [arXiv:hep-ph/0107144],"
      print *, " [arXiv:1309.6323]."
      if ( nloop > 0 ) then
        print *, " Numerical NLL solutions calculated with TOPPIK [arXiv:hep-ph/9904468]"
        print *, " by M. Jezabek, T. Teubner."
      end if
    end if
    print *, "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"
    print *, " "
    if (test) return
    call scan_formfactor_over_pt_en ()
    scan_threshold = .false.
  end subroutine init_threshold_grid

  subroutine update_soft_parameters (sqrts)
    real(default), intent(in) :: sqrts
    real(default) :: nusoft, z, w
    integer :: i
    if ( .not. nustar_dynamic .and. mtpole > 0.0_default ) return
    if ( init .and. is_equal(sqrts, sqrts_ref) ) return
    sqrts_ref = sqrts
    !!! (ultra)soft scales and alphas values required by threshold code
    nusoft = f * nustar (sqrts)
    mu_s = m1s * h * nusoft
    mu_u = m1s * h * nusoft**2
    as   = alphas_soft (sqrts, nloop)
    asLL = alphas_soft (sqrts, 0)
    au   = running_as (mu_u, ah, mu_h, 0, nf) !!! LL here
    !!! *global* pole mass (threshold code)
    mtpole = m1s_to_mpole (sqrts)
    !!! Coulomb potential coefficients needed by TOPPIK
    xc(0) = 1.0_default
    xc(1) = 0.0_default
    xc(2) = 0.0_default
    if ( nloop > 0 ) then
      xc(0) = xc(0) + as/(4.*pi) * a1
      xc(1) = xc(1) + as/(4.*pi) * b0
    end if
    if ( nloop > 1 ) then
      xc(0) = xc(0) + (as/(4.*pi))**2 * a2
      xc(1) = xc(1) + (as/(4.*pi))**2 * (b1 + 2*b0*a1)
      xc(2) = xc(2) + (as/(4.*pi))**2 * b0**2
    end if
    !!! s/p-wave current coefficients, cf. arXiv:hep-ph/0609151, Eq. (62)
    do i=1, 2
      select case (nloop)
        case (0)
          current_c(i) = 1.0_default
        case (1)
          !!! LL here for as, au for consistency!
          z = asLL / ah
          w = au / asLL
          current_c(i) = matching_c(i) * exp( ah*pi*( &
                aa2(i)*(1.-z) + aa3(i)*log(z) + &
                aa4(i)*(1.-z**(1.-13.*CA/(6.*b0))) + &
                aa5(i)*(1.-z**(1.-2.*CA/b0)) + &
                aa8(i)*(1.-z**(1.-CA/b0)) + aa0(i)*(z-1.-log(w)/w) ))
        case default
          print *, "  ERROR: nloop = ", nloop
      end select
    end do
  end subroutine update_soft_parameters

  pure function FF_master (p2, k2, q2, i, ff_in) result (c)
    real(default), intent(in) :: p2
    real(default), intent(in) :: k2
    real(default), intent(in) :: q2
    integer, intent(in) :: i
    integer, intent(in), optional :: ff_in
    complex(default) :: c
    integer :: ff
    real(default) :: sq, p0, pt
    c = 0.0_default
    if ( .not.init ) return
    !!! on-shell veto
    if ( is_equal(sqrt(p2), mtpole_init) .and. is_equal(sqrt(k2), mtpole_init) ) return
    call rel_to_nrel (p2, k2, q2, sq, p0, pt)
    ff = ff_init
    if ( present(ff_in) ) ff = ff_in
    select case (ff)
      case (1)
        c = threshold_formfactor (sq, p0, pt, i)
        return
      case (2)
        !!! not implemented yet: c = continuum_formfactor (p2, k2, q2, i)
        return
      case (3)
        c = approx_formfactor (sq, p0, pt, i)
        return
      case default
        return
    end select
    !!! matching code here ...
  end function FF_master

  pure function FF_threshold (p2, k2, q2, i) result (c)
    real(default), intent(in) :: p2
    real(default), intent(in) :: k2
    real(default), intent(in) :: q2
    integer, intent(in) :: i
    complex(default) :: c
    c = FF_master (p2, k2, q2, i, 1)
  end function FF_threshold

  pure function FF_continuum (p2, k2, q2, i) result (c)
    real(default), intent(in) :: p2
    real(default), intent(in) :: k2
    real(default), intent(in) :: q2
    integer, intent(in) :: i
    complex(default) :: c
    !!! not implemented yet: c = FF_master (p2, k2, q2, i, 2)
    c = 0.0_default
  end function FF_continuum

  pure function FF_approx (p2, k2, q2, i) result (c)
    real(default), intent(in) :: p2
    real(default), intent(in) :: k2
    real(default), intent(in) :: q2
    integer, intent(in) :: i
    complex(default) :: c
    c = FF_master (p2, k2, q2, i, 3)
  end function FF_approx

  pure function threshold_formfactor (sqrts, p0, p, i) result (c)
    real(default), intent(in) :: sqrts
    real(default), intent(in) :: p0
    real(default), intent(in) :: p
    integer, intent(in) :: i
    complex(default) :: c
    real(default) :: en
    c  = 0.0_default
    if ( .not.init ) return
    if ( .not.sqrts_within_range(sqrts) ) return
    en = sqrts - 2.*m1s_to_mpole(sqrts)
    c = formfactor_from_pt_en_scan (p, en, i)
    !!! subtract LO SM contribution ~ 1
    c = c - 1.0_default
  end function threshold_formfactor

  function formfactor_ll_analytic (pt, en, i) result (c)
    real(default), intent(in) :: pt
    real(default), intent(in) :: en
    integer, intent(in) :: i
    complex(default) :: c
    c  = 1.0_default
    if ( .not.init ) return
    select case (i)
      case (1)
        c = (   G0p (    CF * as, mtpole, pt, en, gam) &
              / G0p (0.0_default, mtpole, pt, en, gam)  )
      case (2)
        !!! not implemented yet
        c = 1.0_default
      case default
        print *, "ERROR: unknown ttZ/ttA vertex component i = ", i
    end select
  end function formfactor_ll_analytic

  !!! including p0 dependence
!   function formfactor_ll_analytic_new (pt, en, p0, i) result (c)
!     real(default), intent(in) :: pt
!     real(default), intent(in) :: en
!     real(default), intent(in) :: p0
!     integer, intent(in) :: i
!     complex(default) :: c
!     c  = 1.0_default
!     if ( .not.init .or. i==2 ) return
!     c = (   G0pnew (    CF * as, mtpole, abs(p0), pt, en, gam) &
!           / G0pnew (0.0_default, mtpole, abs(p0), pt, en, gam)  )
!   end function formfactor_ll_analytic_new

  !!! Max's LL nonrelativistic threshold Green's function
  function G0p (a, m, p, en, w) result (c)
    real(default), intent(in) :: a
    real(default), intent(in) :: m
    real(default), intent(in) :: p
    real(default), intent(in) :: en
    real(default), intent(in) :: w
    complex(default) :: c
    complex(default) :: k, ipk, la, z1, z2
    complex(default) :: one, two, cc, dd
    k   = sqrt( -m*en -imago*m*w )
    ipk = imago * p / k
    la  = a * m / 2. / k
    one = 1.
    two = 2.
    cc  = 2. - la
    dd  = ( 1. + ipk ) / 2.
    z1  = nr_hypgeo (two, one, cc, dd)
    dd  = ( 1. - ipk ) / 2.
    z2  = nr_hypgeo (two, one, cc, dd)
    c   = - imago * m / (4.*p*k) / (1.-la) * ( z1 - z2 )
  end function G0p

  !!! including p0 dependence
!   function G0pnew (a, m, P0, p, en, w) result (c)
!     real(default), intent(in) :: a
!     real(default), intent(in) :: m
!     real(default), intent(in) :: P0
!     real(default), intent(in) :: p
!     real(default), intent(in) :: en
!     real(default), intent(in) :: w
!     complex(default) :: c
!     complex(default) :: k, la, z1, z2
!     complex(default) :: aa, bb, cc, dd
!     real(default) :: eps = 1.E-5
!     k  = sqrt( -m*en -imago*m*w )
!     la = a * m / 2. / k
!     aa = eps
!     bb = 1.+eps
!     cc = 1. + eps - la
!     dd = (k-imago*(p-P0)) / (2.*k)
!     z1 = nr_hypgeo (aa, bb, cc, dd)
!     dd = (k+imago*(p+P0)) / (2.*k)
!     z2 = nr_hypgeo (aa, bb, cc, dd)
!     !!! DGamma is Fortran 2008
!     c  = 1.0_default - imago*k*la * &
!            DGamma(eps) * DGamma(1.+eps) * DGamma(real(1.-la)) * &
!            ( -z1 + z2 ) / (p*DGamma(real(cc)))
!     c  = 1.0_default
!   end function G0pnew

  pure function nustar (sqrts) result (nu)
    real(default), intent(in) :: sqrts
    real(default) :: nu
    complex(default) :: arg
    if ( nustar_dynamic ) then
      !!! from [arXiv:1309.6323], Eq. (3.2) (other definitions possible)
      arg = ( sqrts - 2.*m1s + imago*gam ) / m1s
      nu  = nustar_offset + abs(sqrt(arg))
    else
      nu  = nustar_fixed
    end if
  end function nustar

  pure function alphas_soft (sqrts, nl) result (as)
    real(default), intent(in) :: sqrts
    integer, intent(in) :: nl
    real(default) :: as
    real(default) :: musoft
    musoft = m1s * h * f * nustar(sqrts)
    as = running_as (musoft, ah, mu_h, nl, nf)
  end function alphas_soft

  pure function m1s_to_mpole (sqrts, nl_in) result (mpole)
    real(default), intent(in) :: sqrts
    integer, intent(in), optional :: nl_in
    real(default) :: mpole
    integer :: nl
    mpole = mtpole_init
    if ( .not.sqrts_within_range(sqrts) ) return
    nl = nloop
    if ( present(nl_in) ) nl = nl_in
    mpole = m1s * ( 1. + deltaM(sqrts, nl) )
  end function m1s_to_mpole

  pure function mpole_to_m1s (mpole, sqrts, nl) result (m)
    real(default), intent(in) :: mpole
    real(default), intent(in) :: sqrts
    integer, intent(in) :: nl
    real(default) :: m
    m = mpole * ( 1. - deltaM(sqrts, nl) )
  end function mpole_to_m1s

  pure function deltaM (sqrts, nl) result (del)
    real(default), intent(in) :: sqrts
    integer, intent(in) :: nl
    real(default) :: del
    real(default) :: ac
    ac  = CF * alphas_soft (sqrts, nl)
    del = ac**2 / 8.
    if ( nl > 0 ) then
      del = del + ac**3/(8.*pi*CF) * &
                      ( b0*(log(h*f*nustar(sqrts)/ac)+1.) + a1/2. )
    end if
  end function deltaM

  pure function sqrts_within_range (sqrts) result (flag)
    real(default), intent(in) :: sqrts
    logical :: flag
    flag = ( sqrts>2.*m1s+sq_lo .or. sqrts<2.*m1s+sq_hi )
  end function

  pure function sqrts_iter (i_en) result (sqrts)
    integer, intent(in) :: i_en
    real(default) :: sqrts
    sqrts = 2.*m1s + sq_lo + (sq_hi-sq_lo)*real(i_en-1)/real(n_en-1)
  end function sqrts_iter

  pure function is_equal (a, b) result (flag)
    real(default), intent(in) :: a
    real(default), intent(in) :: b
    logical :: flag
    real(single) :: val, acc
    acc = 1.e-6
    val = abs( a/b - 1.0_single )
    flag = ( val < acc )
  end function is_equal

  subroutine scan_formfactor_ll_analytic_over_pt (en, i, i_en)
    real(default), intent(in) :: en
    integer, intent(in) :: i
    integer, intent(in) :: i_en
    integer :: i_pt
    do i_pt=1, n_pt
      pt_data(i_pt) = real(i_pt) / real(n_pt) * mtpole
      ff_data(i_en,i_pt,i) = formfactor_ll_analytic (pt_data(i_pt), en, i)
    end do
    unstable(i_en,i) = .false.
  end subroutine scan_formfactor_ll_analytic_over_pt

  !!! tttoppik wrapper
  subroutine scan_formfactor_toppik_over_pt (en, i, i_en)
    real(default), intent(in) :: en
    integer, intent(in) :: i
    integer, intent(in) :: i_en
    integer :: i_pt

    real*8 :: xenergy, xtm, xtg, xalphas, xscale, xc0, xc1, xc2, xim, xdi, &
        xcutn, xcutv, xkincm, xkinca, xkincv, xcdeltc, &
        xcdeltl, xcfullc, xcfulll, xcrm2
    integer, parameter :: nmax=400
    real*8 :: xdsdp(nmax), xpp(nmax), xww(nmax)
    complex*16 :: ff_toppik(nmax)
    integer :: np, jknflg, jgcflg, jvflg

    if ( n_pt > nmax-40 ) then
      print *, "  ERROR: n_pt must be <=", nmax-40
      return
    end if

    xenergy = en
    xtm     = mtpole
    xtg     = gam
    xalphas = as
    xscale  = mu_s
    xcutn   = 175.E6
    xcutv   = 175.E6
    xc0     = xc(0)
    xc1     = xc(1)
    xc2     = xc(2)
    xcdeltc = 0.
    xcdeltl = 0.
    xcfullc = 0.
    xcfulll = 0.
    xcrm2   = 0.
    xkincm  = 0.
    xkinca  = 0.
    jknflg  = 0
    jgcflg  = 0
    xkincv  = 0.
    jvflg   = 0

    select case (i)
      case (1)
        call tttoppik &
               (xenergy,xtm,xtg,xalphas,xscale,xcutn,xcutv,xc0,xc1,xc2, &
                xcdeltc,xcdeltl,xcfullc,xcfulll,xcrm2,xkincm,xkinca,jknflg, &
                jgcflg, xkincv,jvflg,xim,xdi,np,xpp,xww,xdsdp,ff_toppik)
      case (2)
        call tttoppikaxial &
               (xenergy,xtm,xtg,xalphas,xscale,xcutn,xcutv,xc0,xc1,xc2, &
                xcdeltc,xcdeltl,xcfullc,xcfulll,xcrm2,xkincm,xkinca,jknflg, &
                jgcflg, xkincv,jvflg,xim,xdi,np,xpp,xww,xdsdp,ff_toppik)
        !!! 1st ~10 TOPPIK p-wave entries are unstable: discard them
        ff_toppik(1:10) = (/ (ff_toppik(11), i_pt=1, 10) /)
      case default
        print *, "ERROR: unknown ttZ/ttA vertex component i = ", i
        return
    end select

    pt_data(:) = xpp(1:n_pt)
    ff_data(i_en,:,i) = current_c(i) * ff_toppik(1:n_pt)
    unstable(i_en,i) = ( np < 0 )
  end subroutine scan_formfactor_toppik_over_pt

  subroutine scan_formfactor_over_pt (en, i, i_en_in)
    real(default), intent(in) :: en
    integer, intent(in) :: i
    integer, intent(in), optional :: i_en_in
    complex(default) :: c
    integer :: i_en
    i_en = 1
    if ( present(i_en_in) ) i_en = i_en_in
    select case (nloop)
      case (0)
!        call scan_formfactor_ll_analytic_over_pt (en, i, i_en)
        call scan_formfactor_toppik_over_pt (en, i, i_en)
      case (1)
        call scan_formfactor_toppik_over_pt (en, i, i_en)
      case default
        print *, "  ERROR: unbounded nloop = ", nloop
    end select
  end subroutine scan_formfactor_over_pt

  subroutine scan_formfactor_over_pt_en ()
    real(default) :: sq, en
    integer :: i_en, i_pt, i, n_nan
    do i_en = n_en, 1, -1
      do i=1, 2
        sq = sqrts_iter (i_en)
        call update_soft_parameters (sq)
        en = sq - 2.*mtpole
        call scan_formfactor_over_pt (en, i, i_en)
        en_data(i_en) = en
        scale_data(i_en) = pt_data(20)
        !!! TOPPIK sometimes becomes unstable and returns NaN:
        !!! repair this by hand by interpolating from adjacent energies
        n_nan = 0
        if ( unstable(i_en,i) ) then
          print *, "WARNING: Caught TOPPIK instability at sqrts = ", sqrts_iter(i_en)
          print *, " "
          n_nan = n_nan + 1
          if ( unstable(i_en-1,i) .or. unstable(i_en+1,i) .or. n_nan > 2 ) then
            print *, "ERROR: Too many TOPPIK instabilities! Check your parameter setup"
            print *, "       or slightly vary the scales sh and/or sf. STOP."
            print *, " "
            stop 1
          end if
          ff_data(i_en,:,i) = &
            (/ (( ff_data(i_en-1,i_pt,i) + ff_data(i_en+1,i_pt,i) ) / 2., i_pt=1, n_pt) /)
        end if
      end do
    end do
    do i_en = n_en, 1, -1
      scale_data(i_en) = scale_data(i_en) / scale_data(1)
    end do
    if ( .not.strictly_monotonous(pt_data) ) then
      print *, "ERROR: pt_data NOT strictly monotonous!"
    end if
    if ( .not.strictly_monotonous(en_data) ) then
      print *, "ERROR: en_data NOT strictly monotonous!"
    end if
  end subroutine scan_formfactor_over_pt_en

  pure function formfactor_from_pt_scan (pt, en, i, i_en_in) result (c)
    real(default), intent(in) :: pt
    real(default), intent(in) :: en
    integer, intent(in) :: i
    integer, intent(in), optional :: i_en_in
    complex(default) :: c
    complex(default), dimension(2) :: ff
    integer :: i_en
    i_en = 1
    if ( present(i_en_in) ) i_en = i_en_in
    call interpolate_linear (pt_data, ff_data(i_en,:,:), pt, ff)
    c = ff(i)
  end function formfactor_from_pt_scan

  pure function formfactor_from_pt_en_scan (pt, en, i) result (c)
    real(default), intent(in) :: pt
    real(default), intent(in) :: en
    integer, intent(in) :: i
    complex(default) :: c
    complex(default), dimension(2) :: ff
    real(default) :: scale_pt
    c = 1.0_default
    if ( scan_threshold ) return
    call interpolate_linear (en_data, scale_data, en, scale_pt)
    call interpolate_linear (en_data, pt_data, ff_data, en, pt/scale_pt, ff)
    c = ff(i)
  end function formfactor_from_pt_en_scan

  pure function sqrts_to_v (sqrts) result (v)
    real(default), intent(in) :: sqrts
    complex(default) :: v
    real(default) :: m
    m = m1s_to_mpole (sqrts)
    v = sqrt( (sqrts - 2.*m + imago*gam)/m )
  end function sqrts_to_v

  pure function v_to_sqrts (v) result (sqrts)
    real(default), intent(in) :: v
    real(default) :: sqrts
    real(default) :: m
    m = mtpole_init
    sqrts = 2.*m + m*v**2
  end function v_to_sqrts

  !!! convert 4-momenta into sqrts, p0 = E_top-sqrts/2 and top abs. 3-momentum
  pure subroutine rel_to_nrel (p2, k2, q2, sq, p0, pt)
    real(default), intent(in) :: p2
    real(default), intent(in) :: k2
    real(default), intent(in) :: q2
    real(default), intent(out) :: sq
    real(default), intent(out) :: p0
    real(default), intent(out) :: pt
    sq = sqrt(q2)
    p0 = (p2 - k2) / (2.*sq)
    pt = sqrt( 0.5_default*(-p2 - k2 + sq**2/2. + 2.*p0**2) )
  end subroutine rel_to_nrel

  !!! leading alphas^1 contribution incl. hard coefficient (-> no resummation)
  pure function approx_formfactor (sqrts, p0, p, i) result (c)
    real(default), intent(in) :: sqrts
    real(default), intent(in) :: p0
    real(default), intent(in) :: p
    integer, intent(in) :: i
    complex(default) :: c
    real(default) :: m
    complex(default) :: v
    complex(default) :: i0
    i0 = imago * 1.d-14
    c = 0.0_default
    if ( (.not.init) .or. (i==2) ) return
    m = m1s_to_mpole (sqrts)
    v = sqrt( (sqrts - 2.*m + imago*gam)/m )
    !!! include p0 dependence
    c = 0.5_default*imago*ah*CF*m*log( (p+m*v+abs(p0))/(-p+m*v+abs(p0)) +i0) / p &
        !!! shift from Max' hard current coefficient c1
        -2.0_default*ah*CF/pi
  end function approx_formfactor

  function compute_gamtot_LO (alphaem, sw, Vtb, mt, Mw, mb) result (wt)
    real(default), intent(in) :: alphaem
    real(default), intent(in) :: sw
    real(default), intent(in) :: Vtb
    real(default), intent(in) :: mt
    real(default), intent(in) :: Mw
    real(default), intent(in) :: mb
    real(default) :: wt
    wt = alphaem/4. * mt/(2.*sw**2) * Vtb**2 * ((mt**2 + mb**2)/(2.*mt**2) + &
         (mt**2 - mb**2)**2/(2.*mt**2*Mw**2) - Mw**2/mt**2) / mt**2 * &
         sqrt((mt**2 - (Mw + mb)**2) * (mt**2 - (Mw - mb)**2))
  end function compute_gamtot_LO
end module ttv_formfactors
