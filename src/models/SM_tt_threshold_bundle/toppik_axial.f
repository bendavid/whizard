! WHIZARD <<Version>> <<Date>>

! TOPPIK code by M. Jezabek, T. Teubner (v1.1, 1992), T. Teubner (1998)
!
! NOTE: axial part (p-wave) only
!
! FB: -commented out numerical recipes code for hypergeometric 2F1
!      included in hypgeo.f90;
!     -replaced function 'cdabs' by 'abs';
!     -replaced function 'dabs' by 'abs';
!     -replaced function 'dimag' by 'aimag';
!     -replaced function 'dcmplx(,)' by 'cmplx(,,kind=kind(0d0))';
!     -replaced function 'dreal' by 'real';
!     -replaced function 'dlog' by 'log';
!     -replaced function 'dsqrt' by 'sqrt';
!     -renamed function 'a' to 'aax'
!     -renamed function 'fretil1' to 'fretil1ax'
!     -renamed function 'fretil2' to 'fretil2ax'
!     -renamed function 'fimtil1' to 'fimtil1ax'
!     -renamed function 'fimtil2' to 'fimtil2ax'
!     -renamed function 'freal' to 'frealax'
!     -renamed function 'fim' to 'fimax'
!     -renamed subroutine 'vhat' to 'vhatax'
!     -renamed subroutine 'sae' to 'saeax'
!     -commented out many routines identically defined in 'toppik.f'
!     -modified 'tttoppikaxial' to catch unstable runs.
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

c *********************************************************************
c
c Working version with all the different original potentials
c  like (p^2+q^2)/|p-q|^2, not transformed in terms of delta and 1/r^2;
c accuracy eps=1.d-3 possible (only), but should be save, 13.8.'98, tt.
c cleaned up a bit, 24.2.1999, tt.
c
c *********************************************************************
c
	subroutine tttoppikaxial(xenergy,xtm,xtg,xalphas,xscale,xcutn,
     u     xcutv,
     u     xc0,xc1,xc2,xcdeltc,xcdeltl,xcfullc,xcfulll,xcrm2,
     u     xkincm,xkinca,jknflg,jgcflg,xkincv,jvflg,
     u     xim,xdi,np,xpp,xww,xdsdp,zftild)
c
c *********************************************************************
c
c !! THIS IS NOT A PUBLIC VERSION !!
c
c !!! Only P wave result given as output!!! 9.4.1999, tt.
c
c -- Calculation of the Green function in momentum space by solving the
c     Lippmann-Schwinger equation
c     F(p) = G_0(p) + G_0(p) int_0^xcutn V(p,q) q.p/p^2 F(q) dq
c
c -- Written by Thomas Teubner, Hamburg, November 1998
c     * Based on TOPPIK Version 1.1
c        from M. Jezabek and TT, Karlsruhe, June 1992
c     * Version originally for non-constant top-width
c     * Constant width supplied here
c     * No generator included
c
c -- Use of double precision everywhere
c
c -- All masses, momenta, energies, widths in GeV
c
c -- Input parameters:
c
c    xenergy  :  E=Sqrt[s]-2*topmass
c    xtm      :  topmass (in the Pole scheme)
c    xtg      :  top-width
c    xalphas  :  alpha_s^{MSbar,n_f=5}(xscale)
c    xscale   :  soft scale  mu_{soft}
c    xcutn    :  numerical UV cutoff on all momenta
c                (UV cutoff of the Gauss-Legendre grid)
c    xcutv    :  renormalization cutoff on the
c                 delta-, the (p^2+q^2)/(p-q)^2-, and the
c                  1/r^2-[1/|p-q|]-potential:
c                 if (max(p,q).ge.xcutv) then the three potentials
c                  are set to zero in the Lippmann-Schwinger equation
c    xc0      :  0th order coefficient for the Coulomb potential,
c                 see calling example above
c    xc1      :  1st order coefficient for the Coulomb potential
c    xc2      :  2nd order coefficient for the Coulomb potential
c    xcdeltc  :  constant of the delta(r)-
c                 [= constant in momentum space-] potential
c    xcdeltl  :  constant for the additional log(q^2/mu^2)-part of the
c                 delta-potential:
c                  xcdeltc*1 + xcdeltl*log(q^2/mu^2)
c    xcfullc  :  constant of the (p^2+q^2)/(p-q)^2-potential
c    xcfulll  :  constant for the additional log(q^2/mu^2)-part of the
c                 (p^2+q^2)/(p-q)^2-potential
c    xcrm2    :  constant of the 1/r^2-[1/|p-q|]-potential
c    xkincm   :  } kinetic corrections in the 0th order Green function:
c    xkinca   :  }  G_0(p):=1/[E+iGamma_t-p^2/m_t]*(1+xkincm)+xkinca
c     !!! WATCH THE SIGN IN G_0 !!!
c    jknflg   :  flag for these kinetic corrections:
c                 0 : no kinetic corrections applied
c                 1 : kinetic corrections applied with cutoff xcutv
c                      for  xkinca  only
c                 2 : kinetic corrections applied with cutoff xcutv
c                      for  xkinca  AND  xkincm
c    jgcflg   :  flag for G_0(p) in the LS equation:
c                 0 (standard choice) : G_0(p) as given above
c                 1 (for TIPT)        : G_0(p) = G_c^{0}(p) the 0th
c                                        order Coulomb Green function
c                                        in analytical form; not for
c                                        momenta  p > 1000*topmass
c    xkincv   :  additional kinematic vertexcorrection in G_0, see below:
c    jvflg    :  flag for the additional vertexcorrection  xkincv  in the
c                 ``zeroth order'' G_0(p) in the LS-equation:
c                 0 : no correction, means  G = G_0 + G_0 int V G
c                      with G_0=1/[E+iGamma_t-p^2/m_t]*(1+xkincm)+xkinca
c                 1 : apply the correction in the LS equation as
c                      G = G_0 + xkincv*p^2/m_t^2/[E+iGamma_t-p^2/m_t] + 
c                          G_0 int V G
c                     and correct the integral over Im[G(p)] to get sigma_tot
c                     from the optical theorem by the same factor.
c                     The cutoff  xcutv  is applied for these corrections.
c
c -- Output:
c
c    xim      :  R^{P wave}_{ttbar} from the imaginary part of the Green
c                 function
c    xdi      :  R^{P wave}_{ttbar} from the integral over the momentum
c                 distribution: int_0^xcutv dp p^3/m_t*|F(p,E)|^2
c    np       :  number of points used for the grid; fixed in tttoppik
c    xpp      :  1-dim array (max. 400 elements) giving the momenta of
c                 the Gauss-Legendre grid (pp(i) in the code)
c    xww      :  1-dim array (max. 400 elements) giving the corresponding
c                 Gauss-Legendre weights for the grid
c    xdsdp    :  1-dim array (max. 400 elements) giving the
c                 momentum distribution of top: d\sigma^{P wave}/dp,
c                  normalized to R,
c                  at the momenta of the Gauss-Legendre grid xpp(i)
c    zftild   :  1-dim array (max. 400 elements) of COMPLEX*16 numbers
c                 giving the vertex function K_A for the P-wave
c                 at the momenta of the grid.
c                 Then F(p)=K_A (p)*G_0(p) corresponding to G=K_V*G_0.
c
c *********************************************************************
c
c
	   implicit none
	   real*8
     u        pi,energy,vzero,eps,
     u        pp,
     u        tmass,tgamma,zmass,alphas,alamb5,
     u        wmass,wgamma,bmass,GFERMI,
     u	      xx,critp,consde,
     u        w1,w2,sig1,sig2,const,
     u        gtpcor,etot,
     u        xenergy,xtm,xtg,xalphas,xscale,xc0,xc1,xc2,xim,xdi,
     u        xaai,xaad,xdsdp,xpp,xww,
     u        cplas,scale,c0,c1,c2,cdeltc,cdeltl,cfullc,cfulll,crm2,
     u        xcutn,dcut,xcutv,
     u        xp,xpmax,
     u        kincom,kincoa,kincov,xkincm,xkinca,xkincv,
     u        xcdeltc,xcdeltl,xcfullc,xcfulll,xcrm2
	   complex*16 bb,vec,gg,a1,aax,g0,g0c,zvfct,zftild
	   integer i,n,nmax,npot,np,gcflg,kinflg,jknflg,jgcflg,
     u             jvflg,vflag
	   parameter (nmax=400)
	   dimension pp(nmax),bb(nmax),vec(nmax),xx(nmax),gg(nmax),
     u               w1(nmax),w2(nmax),a1(nmax),
     u               xdsdp(nmax),xpp(nmax),xww(nmax),
     u               zvfct(nmax),zftild(nmax)
c
	   external aax,gtpcor,g0,g0c
c
	   common/ovalco/ pi, energy, vzero, eps, npot
      COMMON/PHCONS/TMASS,TGAMMA,ZMASS,ALPHAS,ALAMB5,
     $ WMASS,WGAMMA,BMASS,GFERMI
	   common/cplcns/cplas,scale,c0,c1,c2,
     u                   cdeltc,cdeltl,cfullc,cfulll,crm2
	   common/mom/ xp,xpmax,dcut
	   common/g0inf/kincom,kincoa,kincov,kinflg,gcflg,vflag
c
	   pi=3.141592653589793238d0
c
c Number of points to evaluate on the integral equation
c  (<=400 and n mod 3 = 0 !!):
	   n=360
	   np=n
c
c For second order potential with free parameters:
c
	   npot=5
c Internal accuracy for TOPPIK, the reachable limit may be smaller,
c  depending on the parameters. But increase in real accuracy only
c  in combination with large number of points.
	   eps=1.d-3
c Some physical parameters:
	   wgamma=2.07d0
	   zmass=91.187d0
	   wmass=80.33d0
	   bmass=4.7d0
c
c Input:
	   tmass=xtm
	   energy=xenergy
	   tgamma=xtg
	   cplas=xalphas
	   scale=xscale
	   c0=xc0
	   c1=xc1
	   c2=xc2
	   cdeltc=xcdeltc
	   cdeltl=xcdeltl
	   cfullc=xcfullc
	   cfulll=xcfulll
	   crm2=xcrm2
	   kincom=xkincm
	   kincoa=xkinca
	   kincov=xkincv
	   kinflg=jknflg
	   gcflg=jgcflg
	   vflag=jvflg
c
	   alphas=xalphas
c
c Cut for divergent potential-terms for large momenta in the function vhatax
c  and in the integrals aax(p):
	   dcut=xcutv
c
c Numerical Cutoff of all momenta (maximal momenta of the grid):
	   xpmax=xcutn
	   if (dcut.gt.xpmax) then
	      write(*,*) ' dcut > xpmax  makes no sense! Stop.'
	      stop
	   endif
c
c Not needed for the fixed order potentials:
	   alamb5=0.2d0
c
c      WRITE(*,*) 'INPUT TGAMMA=',TGAMMA	   
c Needed in subroutine GAMMAT:
	   GFERMI=1.16637d-5
c           CALL GAMMAT
c           WRITE(*,*) 'CALCULATED TGAMMA=',TGAMMA
c
	   etot=2.d0*tmass+energy
c
	   if ((npot.eq.1).or.(npot.eq.3).or.(npot.eq.4).or.
     u         (npot.eq.5)) then
c For pure coulomb and fixed order potentials there is no delta-part:
	      consde = 0.d0
	   else if (npot.eq.2) then
c Initialize QCD-potential common-blocks and calculate constant multiplying
c  the delta-part of the 'qcutted' potential in momentum-space:
c	      call iniphc(1)
c	      call vqdelt(consde)
	      write(*,*) ' Not supplied with this version. Stop.'
	      stop
	   else
	      write (*,*) ' Potential not implemented! Stop. 1'
	      stop
	   endif
c Delta-part of potential is absorbed by subtracting vzero from the
c  original energy (shift from the potential to the free Hamiltonian):
	   vzero = consde / (2.d0*pi)**3
c	   write (*,*) 'vzero=', vzero
c
c Find x-values pp(i) and weigths w1(i) for the gaussian quadrature;
c  care about large number of points in the important intervals:
c	if (energy-vzero.le.0.d0) then
cc	   call gauleg(0.d0, 1.d0, pp, w1, n/3)
cc	   call gauleg(1.d0, 5.d0, pp(n/3+1), w1(n/3+1), n/3)
cc	   call gauleg(0.d0, 0.2d0, pp(2*n/3+1), w1(2*n/3+1), n/3)
c	   call gauleg(0.d0, 5.d0, pp, w1, n/3)
c	   call gauleg(5.d0, 20.d0, pp(n/3+1), w1(n/3+1), n/3)
c	   call gauleg(0.d0, 0.05d0, pp(2*n/3+1), w1(2*n/3+1), n/3)
c	else
cc Avoid numerical singular points in the inner of the intervals:
c	   critp = sqrt((energy-vzero)*tmass)
c	   if (critp.le.1.d0) then
cc Gauss-Legendre is symmetric => automatically principal-value prescription:
c	      call gauleg(0.d0, 2.d0*critp, pp, w1, n/3)
c	      call gauleg(2.d0*critp, 20.d0, pp(n/3+1),
c     u                    w1(n/3+1), n/3)
c	      call gauleg(0.d0, 0.05d0, pp(2*n/3+1), w1(2*n/3+1), n/3)
c	   else
cc Better behaviour at the border of the intervals:
c	      call gauleg(0.d0, critp, pp, w1, n/3)
c	      call gauleg(critp, 2.d0*critp, pp(n/3+1),
c     u                    w1(n/3+1), n/3)
c	      call gauleg(0.d0, 1.d0/(2.d0*critp), pp(2*n/3+1),
c     u                    w1(2*n/3+1), n/3)
c	   endif
c	endif
c
c Or different (simpler) method, good for V_JKT:
	   if (energy.le.0.d0) then
	      critp=tmass/3.d0
	   else
	      critp=max(tmass/3.d0,2.d0*sqrt(energy*tmass))
	   endif
	   call gauleg(0.d0, critp, pp, w1, 2*n/3)
	   call gauleg(1.d0/xpmax, 1.d0/critp, pp(2*n/3+1),
     u                 w1(2*n/3+1), n/3)
c
c Do substitution p => 1/p for the last interval explicitly:
	   do 10 i=2*n/3+1,n
	      pp(i) = 1.d0/pp(i)
10	   continue
c
c Reorder the arrays for the third interval:
	   do 20 i=1,n/3
	      xx(i) = pp(2*n/3+i)
	      w2(i) = w1(2*n/3+i)
20	   continue
	   do 30 i=1,n/3
	      pp(n-i+1) = xx(i)
	      w1(n-i+1) = w2(i)
30	   continue
c
c Calculate the integrals aax(p) for the given momenta pp(i)
c  and store weights and momenta for the output arrays:
	   do 40 i=1,n
	      a1(i) = aax(pp(i)) !!! FB: can get stuck in original Toppik!
	      !!! FB: abuse 'np' as a flag to communicate unstable runs
	      if ( abs(a1(i)) .gt. 1d10 ) then
	        np = -1
	        return
	      endif
	      xpp(i)=pp(i)
	      xww(i)=w1(i)
40	   continue
	   do 41 i=n+1,nmax
	      xpp(i)=0.d0
	      xww(i)=0.d0
41	   continue
c
c Solve the integral-equation by solving a system of algebraic equations:
	   call saeax(pp, w1, bb, vec, a1, n)
c
c (The substitution for the integration to infinity  pp => 1/pp
c  is done already.)
	   do 50 i=1,n
	      zvfct(i)=bb(i)
	      zftild(i)=vec(i)
	      gg(i) = bb(i)*g0c(pp(i))
cc	      gg(i) = (1.d0 + bb(i))*g0c(pp(i))
cc Urspruenglich anderes (Minus) VZ hier, dafuer kein Minus mehr bei der 
cc  Definition des WQs ueber Im G, 2.6.1998, tt.
cc	      gg(i) = - (1.d0 + bb(i))*g0c(pp(i))
50	   continue
c
c Normalisation on R:
	   const = 8.d0*pi/tmass**2
c
c Proove of the optical theorem for the output values of saeax:
c  Simply check if sig1 = sig2.
	   sig1 = 0.d0
	   sig2 = 0.d0
	   xaai = 0.d0
	   xaad = 0.d0
	   do 60 i=1,n*2/3
c	      write(*,*) 'check! p(',i,') = ',pp(i)
cvv
	      if (pp(i).lt.dcut.and.vflag.eq.1) then
		 sig1 = sig1 + w1(i)*pp(i)**2*aimag(gg(i)
cc     u                 *(1.d0+kincov*(pp(i)/tmass)**2)
     u   *(1.d0+kincov*g0(pp(i))*(pp(i)/tmass)**2/g0c(pp(i)))
     u                  )
	      else
		 sig1 = sig1 + w1(i)*pp(i)**2*aimag(gg(i))
	      endif
	      if (pp(i).lt.dcut.and.kinflg.ne.0) then
   	         sig2 = sig2 + w1(i)*pp(i)**2*abs(gg(i))**2 *
     u                  tgamma*gtpcor(pp(i),etot)
     u                  *(1.d0-pp(i)**2/2.d0/tmass**2)
cc     u                  *tmass/sqrt(tmass**2+pp(i)**2)
c		 xdsdp(i)=pp(i)**2*abs(gg(i))**2 *
c     u                  tgamma*gtpcor(pp(i),etot)
c     u                  *(1.d0-pp(i)**2/2.d0/tmass**2)
c     u                  /(2.d0*pi**2)*const
              else
   	         sig2 = sig2 + w1(i)*pp(i)**2*abs(gg(i))**2 *
     u                  tgamma*gtpcor(pp(i),etot)
c		 xdsdp(i)=pp(i)**2*abs(gg(i))**2 *
c     u                  tgamma*gtpcor(pp(i),etot)
c     u                  /(2.d0*pi**2)*const
	      endif
	      xdsdp(i)=pp(i)**4/tmass**2*abs(zftild(i)*g0c(pp(i)))**2
     u                 *tgamma*gtpcor(pp(i),etot)
     u                 /(2.d0*pi**2)*const
	      xaai=xaai+w1(i)*pp(i)**4/tmass**2*
     u                  aimag(zftild(i)*g0c(pp(i)))
	      xaad=xaad+w1(i)*pp(i)**4/tmass**2*
     u                  abs(zftild(i)*g0c(pp(i)))**2 *
     u                  tgamma*gtpcor(pp(i),etot)
c	      write(*,*) 'xdsdp = ',xdsdp(i)
c	      write(*,*) 'zvfct = ',zvfct(i)
c	      write(*,*) 'zftild = ',zftild(i)
60	   continue
c '*p**2' because of substitution p => 1/p in the integration of p**2*G(p)
c  to infinity
	   do 70 i=n*2/3+1,n
c	      write(*,*) 'check! p(',i,') = ',pp(i)
cvv
	      if (pp(i).lt.dcut.and.vflag.eq.1) then
		 sig1 = sig1 + w1(i)*pp(i)**4*aimag(gg(i)
cc     u                 *(1.d0+kincov*(pp(i)/tmass)**2)
     u   *(1.d0+kincov*g0(pp(i))*(pp(i)/tmass)**2/g0c(pp(i)))
     u                  )
	      else
		 sig1 = sig1 + w1(i)*pp(i)**4*aimag(gg(i))
	      endif
	      if (pp(i).lt.dcut.and.kinflg.ne.0) then
	         sig2 = sig2 + w1(i)*pp(i)**4*abs(gg(i))**2 *
     u                  tgamma*gtpcor(pp(i),etot)
     u                  *(1.d0-pp(i)**2/2.d0/tmass**2)
cc     u                  *tmass/sqrt(tmass**2+pp(i)**2)
c		 xdsdp(i)=pp(i)**2*abs(gg(i))**2 *
c     u                  tgamma*gtpcor(pp(i),etot)
c     u                  *(1.d0-pp(i)**2/2.d0/tmass**2)
c     u                  /(2.d0*pi**2)*const
	      else
	         sig2 = sig2 + w1(i)*pp(i)**4*abs(gg(i))**2 *
     u                  tgamma*gtpcor(pp(i),etot)
c                 xdsdp(i)=pp(i)**2*abs(gg(i))**2 *
c     u                  tgamma*gtpcor(pp(i),etot)
c     u                  /(2.d0*pi**2)*const
	      endif
              xdsdp(i)=pp(i)**4/tmass**2*abs(zftild(i)*g0c(pp(i)))**2
     u                 *tgamma*gtpcor(pp(i),etot)
     u                 /(2.d0*pi**2)*const
	      xaai=xaai+w1(i)*pp(i)**6/tmass**2*
     u                  aimag(zftild(i)*g0c(pp(i)))
	      xaad=xaad+w1(i)*pp(i)**6/tmass**2*
     u                  abs(zftild(i)*g0c(pp(i)))**2 *
     u                  tgamma*gtpcor(pp(i),etot)
c	      write(*,*) 'xdsdp = ',xdsdp(i)
c	      write(*,*) 'zvfct = ',zvfct(i)
c	      write(*,*) 'zftild = ',zftild(i)
70	   continue
	   do 71 i=n+1,nmax
	     xdsdp(i)=0.d0
	     zvfct(i)=(0.d0,0.d0)
	     zftild(i)=(0.d0,0.d0)
71	   continue
c
c Normalisation on R:
	   sig1  = sig1 / (2.d0*pi**2) * const
	   sig2  = sig2 / (2.d0*pi**2) * const
c
c The results from the momentum space approach finally are: 
cc Jetzt Minus hier, 2.6.98, tt.
c	   xim=-sig1
c	   xdi=sig2
	   xaai=-xaai / (2.d0*pi**2) * const
	   xaad=xaad / (2.d0*pi**2) * const
c Output of P wave part only:
	   xim=xaai
	   xdi=xaad
c	   write(*,*) 'vvi = ',-sig1,' .  vvd = ',sig2
c	   write(*,*) 'aai = ',xim,' .  aad = ',xdi
c
	end
c
c
!!! FB:
! 	complex*16 function g0(p)
! c
! 	   implicit none
! 	   real*8
!      u        tmass,tgamma,zmass,alphas,alamb5,
!      u        wmass,wgamma,bmass,GFERMI,
!      u        pi,energy,vzero,eps,
!      u        p,gtpcor
! 	   integer npot
!       COMMON/PHCONS/TMASS,TGAMMA,ZMASS,ALPHAS,ALAMB5,
!      $ WMASS,WGAMMA,BMASS,GFERMI
! 	   common/ovalco/ pi, energy, vzero, eps, npot
! 	   external gtpcor
! 	   save
! 	   g0=1.d0/cmplx(energy-vzero-p**2/tmass,
!      u                    tgamma*gtpcor(p,2.d0*tmass+energy),
!      u                    kind=kind(0d0))
! 	end
! c
! 	complex*16 function g0c(p)
! c
! 	   implicit none
! 	   complex*16 hypgeo,green,zk,zi,amd2k,aa,bb,cc,zzp,zzm,
!      u                hypp,hypm,g0
! 	   real*8
!      u        tmass,tgamma,zmass,alphas,alamb5,
!      u        wmass,wgamma,bmass,GFERMI,
!      u        pi,energy,vzero,eps,
!      u        p,gtpcor,
!      u        kincom,kincoa,kincov,xp,xpmax,dcut
! 	   integer npot,kinflg,gcflg,vflag
!       COMMON/PHCONS/TMASS,TGAMMA,ZMASS,ALPHAS,ALAMB5,
!      $ WMASS,WGAMMA,BMASS,GFERMI
! 	   common/ovalco/ pi, energy, vzero, eps, npot
! 	   common/g0inf/kincom,kincoa,kincov,kinflg,gcflg,vflag
! 	   common/mom/ xp,xpmax,dcut
! 	   external hypgeo,gtpcor,g0
! 	   save
! c
! 	   if (gcflg.eq.0) then
! 	      if (kinflg.eq.0) then
! 		 g0c=g0(p)
! 	      else if (kinflg.eq.1.and.p.lt.dcut) then
! 		 g0c=g0(p)*(1.d0+kincom)+kincoa
! 	      else if (kinflg.eq.1.and.p.ge.dcut) then
! 		 g0c=g0(p)*(1.d0+kincom)
! 	      else if (kinflg.eq.2.and.p.lt.dcut) then
! 		 g0c=g0(p)*(1.d0+kincom)+kincoa
! 	      else if (kinflg.eq.2.and.p.ge.dcut) then
! 		 g0c=g0(p)
! 	      else
! 		 write(*,*) ' kinflg wrong! Stop.'
! 		 stop
! 	      endif
! 	   else if (gcflg.eq.1) then
! 	      zi=(0.d0,1.d0)
! 	      zk=-tmass*cmplx(energy,tgamma
!      u                         *gtpcor(p,2.d0*tmass+energy),
!      u                         kind=kind(0d0))
! 	      zk=sqrt(zk)
! 	      amd2k=4.d0/3.d0*alphas*tmass/2.d0/zk
! 	      aa=(2.d0,0.d0)
! 	      bb=(1.d0,0.d0)
! 	      cc=2.d0-amd2k
! 	      zzp=(1.d0+zi*p/zk)/2.d0
! 	      zzm=(1.d0-zi*p/zk)/2.d0
! 	      if (abs(zzp).gt.20.d0) then
! 		 hypp=(1.d0-zzp)**(-aa)*
!      u                hypgeo(aa,cc-bb,cc,zzp/(zzp-1.d0))
! 	      else
! 		 hypp=hypgeo(aa,bb,cc,zzp)
! 	      endif
! 	      if (abs(zzm).gt.20.d0) then
! 		 hypm=(1.d0-zzm)**(-aa)*
!      u                hypgeo(aa,cc-bb,cc,zzm/(zzm-1.d0))
! 	      else
! 		 hypm=hypgeo(aa,bb,cc,zzm)
! 	      endif
! 	      green=-zi*tmass/(4.d0*p*zk)/(1.d0-amd2k)*(hypp-hypm)
! c VZ anders herum als in Andres Konvention, da bei ihm G_0=1/[-E-i G+p^2/m]:
! 	      g0c=-green
! 	      if (p.gt.1.d3*tmass) then
! 		 write(*,*) ' g0cana = ',g0c,' not reliable. Stop.'
! 		 stop
! 	      endif
! 	   else
! 	      write(*,*) ' gcflg wrong! Stop.'
! 	      stop
! 	   endif
! c
! 	end
!!! /FB
c
c
	complex*16 function aax(p)
c
	   implicit none
	   real*8
     u        tmass,tgamma,zmass,alphas,alamb5,
     u        wmass,wgamma,bmass,GFERMI,
     u        pi, energy,vzero, eps,
     $        QCUT,QMAT1,ALR,
c     u       ,PCUT,etot
     u        p,
     u        xp,xpmax, xb1,xb2,dcut,ddcut,
     u        a1, a2, a3, a4,a5,a6,
     u        adglg1, fretil1ax, fretil2ax, fimtil1ax, fimtil2ax,
     u        ALEFVQ, gtpcor, ad8gle, buf,adglg2,
c     u        xerg,
     u        kincom,kincoa,kincov
	   complex*16 zapvq1
c     u               ,ZAPVGP
	   integer npot,ILFLAG,kinflg,gcflg,vflag
c
      COMMON/PHCONS/TMASS,TGAMMA,ZMASS,ALPHAS,ALAMB5,
     $ WMASS,WGAMMA,BMASS,GFERMI
      COMMON/PARFLG/ QCUT,QMAT1,ALR,ILFLAG
	   common/ovalco/ pi, energy, vzero, eps, npot
	   common/mom/ xp,xpmax,dcut
	   common/g0inf/kincom,kincoa,kincov,kinflg,gcflg,vflag
c
	   external adglg1, fretil1ax, fretil2ax, fimtil1ax, fimtil2ax,
     u              zapvq1, ALEFVQ, gtpcor,ad8gle,adglg2
c     u             ,zapvgp
c
	   if ((npot.eq.1).or.(npot.eq.3).or.(npot.eq.4).or.
     u         (npot.eq.5)) then
c
	      xp=p
	      buf=0.d0
c
	      a1=0.d0
	      a2=0.d0
	      a3=0.d0
	      a4=0.d0
	      a5=0.d0
	      a6=0.d0
	      if (gcflg.eq.0) then
		 ddcut=xpmax
	      else if (gcflg.eq.1) then
		 ddcut=dcut
	      else
		 write(*,*) ' gcflg wrong! Stop.'
		 stop
	      endif
c
	      if (2.d0*xp.lt.ddcut) then
		 xb1=xp
		 xb2=2.d0*xp
c
c More stable for logarithmically divergent fixed order potentials:
c
	         a1=adglg1(fretil1ax, buf, xb1, eps)
	         a2=adglg1(fimtil1ax, buf, xb1, eps)
c Slightly unstable:
		 a3=adglg2(fretil1ax,xb1,xb2,eps)
c No good:
c		 a3=adglg1(fretil1ax,xb1,xb2,eps)
c Not better:
c		 call adqua(xb1,xb2,fretil1ax,xerg,eps)
c		 a3=xerg
c Also not better:
c	         a1=adglg1(fretil1ax, buf, xb2, eps)		 
c
		 a4=adglg2(fimtil1ax,xb1,xb2,eps)
c		 a5 = adglg2(fretil1ax, xb2, ddcut, eps)
c		 a6 = adglg2(fimtil1ax, xb2, ddcut, eps)
		 a5 = adglg2(fretil2ax, 1.d0/ddcut, 1.d0/xb2, eps)
		 a6 = adglg2(fimtil2ax, 1.d0/ddcut, 1.d0/xb2, eps)
	      else if (xp.lt.ddcut) then
		 xb1=xp
		 xb2=ddcut
	         a1=adglg1(fretil1ax, buf, xb1, eps)
	         a2=adglg1(fimtil1ax, buf, xb1, eps)
		 a3=adglg2(fretil1ax,xb1,xb2,eps)
		 a4=adglg2(fimtil1ax,xb1,xb2,eps)
              else if (ddcut.le.xp) then
              else
		 write(*,*) ' Constellation not possible! Stop.'
		 stop
	      endif
c
	      aax  = 1.d0/(4.d0*pi**2)*cmplx(a1+a3+a5,a2+a4+a6,
     u                    kind=kind(0d0))
c
c	   else if (npot.eq.2) then
c      PCUT=QCUT
c      ETOT=ENERGY+2*TMASS
c	      aax  = ZAPVGP(P,ETOT,VZERO-ENERGY,PCUT,EPS)
	   else
	      write (*,*) ' Potential not implemented! Stop. 2'
	      stop
	   endif
c
	end
c
	real*8 function fretil1ax(xk)
	   implicit none
	   real*8 xk, frealax
	   external frealax
	   fretil1ax = frealax(xk)
	end
c
	real*8 function fretil2ax(xk)
	   implicit none
	   real*8 xk, frealax
	   external frealax
	   fretil2ax = frealax(1.d0/xk) * xk**(-2)
	end
c
	real*8 function fimtil1ax(xk)
	   implicit none
	   real*8 xk, fimax
	   external fimax
	   fimtil1ax = fimax(xk)
	end
c
	real*8 function fimtil2ax(xk)
	   implicit none
	   real*8 xk, fimax
	   external fimax
	   fimtil2ax = fimax(1.d0/xk) * xk**(-2)
	end
c
	real*8 function frealax(xk)
	   implicit none
	   complex*16 vhatax
	   real*8
     u        tmass,tgamma,zmass,alphas,alamb5,
     u        wmass,wgamma,bmass,GFERMI,
     u        pi, energy, vzero, eps,
     u        p,pmax, xk, gtpcor,dcut
	   complex*16 g0,g0c
	   integer npot
      COMMON/PHCONS/TMASS,TGAMMA,ZMASS,ALPHAS,ALAMB5,
     $ WMASS,WGAMMA,BMASS,GFERMI
	   common/ovalco/ pi, energy, vzero, eps, npot
	   common/mom/ p,pmax,dcut
	   external vhatax, g0, g0c, gtpcor
c
	   frealax = real(g0c(xk)*vhatax(p, xk))
	end
c
	real*8 function fimax(xk)
	   implicit none
	   complex*16 vhatax
	   real*8
     u        tmass,tgamma,zmass,alphas,alamb5,
     u        wmass,wgamma,bmass,GFERMI,
     u        pi, energy, vzero, eps,
     u        p,pmax, xk, gtpcor,dcut
	   complex*16 g0,g0c
	   integer npot
      COMMON/PHCONS/TMASS,TGAMMA,ZMASS,ALPHAS,ALAMB5,
     $ WMASS,WGAMMA,BMASS,GFERMI
	   common/ovalco/ pi, energy, vzero, eps, npot
	   common/mom/ p,pmax,dcut
	   external vhatax, g0, g0c, gtpcor
	   fimax = aimag(g0c(xk)*vhatax(p, xk))
	end
c
c
	complex*16 function vhatax(p, xk)
c
	   implicit none
	   complex*16 zi
	   real*8
     u        tmass,tgamma,zmass,alphas,alamb5,
     u        wmass,wgamma,bmass,GFERMI,
     u        pi, energy, vzero, eps,
     u        p, xk,
     u        cnspot, phiint, AD8GLE,
     u        pm, xkm,
c     u        phfqcd, ALPHEF,
     u        zeta3,cf,ca,tf,xnf,a1,a2,b0,b1,
     u        cplas,scale,c0,c1,c2,
     u        cdeltc,cdeltl,cfullc,cfulll,crm2,
     u        xkpln1st,xkpln2nd,xkpln3rd,
     u        pp,pmax,dcut
	   integer npot
	   parameter(zi=(0.d0,1.d0))
	   parameter(zeta3=1.20205690316d0,
     u               cf=4.d0/3.d0,ca=3.d0,tf=1.d0/2.d0,
     u               xnf=5.d0)
c
	   external AD8GLE
c     u            , phfqcd, ALPHEF
c
      COMMON/PHCONS/TMASS,TGAMMA,ZMASS,ALPHAS,ALAMB5,
     $ WMASS,WGAMMA,BMASS,GFERMI
	   common/ovalco/ pi, energy, vzero, eps, npot
	   common/pmaxkm/ pm, xkm
	   common/mom/ pp,pmax,dcut
	   common/cplcns/cplas,scale,c0,c1,c2,
     u                   cdeltc,cdeltl,cfullc,cfulll,crm2
c
	   b0=11.d0-2.d0/3.d0*xnf
	   b1=102.d0-38.d0/3.d0*xnf
c
	   a1=31.d0/9.d0*ca-20.d0/9.d0*tf*xnf
	   a2=(4343.d0/162.d0+4.d0*pi**2-pi**4/4.d0+
     u         22.d0/3.d0*zeta3)*ca**2-
     u        (1798.d0/81.d0+56.d0/3.d0*zeta3)*ca*tf*xnf-
     u        (55.d0/3.d0-16.d0*zeta3)*cf*tf*xnf+
     u        (20.d0/9.d0*tf*xnf)**2
c
	   pm=p
	   xkm=xk
	   cnspot=-4.d0/3.d0*4.d0*pi
c
	   if (p/xk.le.1.d-5.and.p.le.1.d-5) then
	      xkpln1st=2.d0
	      xkpln2nd=-4.d0*log(scale/xk)
	      xkpln3rd=-6.d0*log(scale/xk)**2
	   else if (xk/p.le.1.d-5.and.xk.le.1.d-5) then
	      xkpln1st=2.d0*(xk/p)**2
	      xkpln2nd=-4.d0*(xk/p)**2*log(scale/p)
	      xkpln3rd=-6.d0*(xk/p)**2*log(scale/p)**2
	   else
c	      xkpln1st=xk/p*log(abs((p+xk)/(p-xk)))
	      xkpln1st=xk/p*(log(p+xk)-log(abs(p-xk)))
	      xkpln2nd=xk/p*(-1.d0)*(log(scale/(p+xk))**2-
     u                               log(scale/abs(p-xk))**2)
	      xkpln3rd=xk/p*(-4.d0/3.d0)*(log(scale/(p+xk))**3-
     u                                    log(scale/abs(p-xk))**3)
	   endif
c
c	   if (npot.eq.2) then
c	      if (p/xk.le.1.d-5.and.p.le.1.d-5) then
c		 vhatax = 2.d0 * cnspot * ALPHEF(xk)
c	      else if (xk/p.le.1.d-5.and.xk.le.1.d-5) then
c		 vhatax = 2.d0 * cnspot * xk**2 / p**2 * ALPHEF(p)
c	      else
c		 phiint = cnspot * (AD8GLE(phfqcd, 0.d0, 0.3d0, 1.d-5)
c     u                            +AD8GLE(phfqcd, 0.3d0, 1.d0, 1.d-5))
c		 vhatax   = xk / p * log(abs((p+xk)/(p-xk))) * phiint
c	      endif
c	   else
	      if (npot.eq.1) then
		 c0=1.d0
		 c1=0.d0
		 c2=0.d0
	      else if (npot.eq.3) then
		 c0=1.d0+alphas/(4.d0*pi)*a1
		 c1=alphas/(4.d0*pi)*b0
		 c2=0
	      else if (npot.eq.4) then
		 c0=1.d0+alphas/(4.d0*pi)*a1+(alphas/(4.d0*pi))**2*a2
		 c1=alphas/(4.d0*pi)*b0+
     u             (alphas/(4.d0*pi))**2*(b1+2.d0*b0*a1)
		 c2=(alphas/(4.d0*pi))**2*b0**2
	      else if (npot.eq.5) then
              else
		 write (*,*) ' Potential not implemented! Stop. 3'
		 stop
	      endif
	      phiint=cnspot*alphas
c
c	      if ((xk+p).le.dcut) then
c		 vhatax=phiint*(c0*xkpln1st+c1*xkpln2nd+c2*xkpln3rd)
c     u               -1.d0/2.d0*(1.d0+2.d0*ca/cf)
c     u                *(pi*cf*alphas)**2/tmass
c     u                *xk/p*(p+xk-abs(xk-p))
c	      else if (abs(xk-p).lt.dcut) then
c		 vhatax=phiint*(c0*xkpln1st+c1*xkpln2nd+c2*xkpln3rd)
c     u               -1.d0/2.d0*(1.d0+2.d0*ca/cf)
c     u                *(pi*cf*alphas)**2/tmass
c     u                *xk/p*(dcut-abs(xk-p))
c	      else if (dcut.le.abs(xk-p)) then
c		 vhatax=phiint*(c0*xkpln1st+c1*xkpln2nd+c2*xkpln3rd)
c	      else
c		 write(*,*) ' Not possible! Stop.'
c		 stop
c	      endif
c
     	      if (max(xk,p).lt.dcut) then
c Coulomb + first + second order corrections:
		 vhatax=phiint*(c0*xkpln1st+c1*xkpln2nd+c2*xkpln3rd)
c All other potentials:
     u               +cdeltc*2.d0*xk**2
     u               +cdeltl*xk/p/2.d0*(
     u                (p+xk)**2*(log(((p+xk)/scale)**2)-1.d0)-
     u                (p-xk)**2*(log(((p-xk)/scale)**2)-1.d0))
     u               +cfullc*(p**2+xk**2)*xkpln1st
     u               +cfulll*(p**2+xk**2)*xk/p/4.d0*
     u                 (log(((p+xk)/scale)**2)**2-
     u                  log(((p-xk)/scale)**2)**2)
     u               +crm2*xk/p*(p+xk-abs(xk-p))
	      else
		 vhatax=phiint*(c0*xkpln1st+c1*xkpln2nd+c2*xkpln3rd)
	      endif
c	   endif
c
	end
c
c
	complex*16 function vhhat(p, xk)
c
	   implicit none
	   complex*16 zi
	   real*8
     u        tmass,tgamma,zmass,alphas,alamb5,
     u        wmass,wgamma,bmass,GFERMI,
     u        pi, energy, vzero, eps,
     u        p, xk,
     u        cnspot, phiint, AD8GLE,
     u        pm, xkm,
     u        zeta3,cf,ca,tf,xnf,a1,a2,b0,b1,
     u        cplas,scale,c0,c1,c2,
     u        cdeltc,cdeltl,cfullc,cfulll,crm2,
     u        xkpln1st,
     u        pp,pmax,dcut
	   integer npot
	   parameter(zi=(0.d0,1.d0))
	   parameter(zeta3=1.20205690316d0,
     u               cf=4.d0/3.d0,ca=3.d0,tf=1.d0/2.d0,
     u               xnf=5.d0)
c
	   external AD8GLE
c
      COMMON/PHCONS/TMASS,TGAMMA,ZMASS,ALPHAS,ALAMB5,
     $ WMASS,WGAMMA,BMASS,GFERMI
	   common/ovalco/ pi, energy, vzero, eps, npot
	   common/pmaxkm/ pm, xkm
	   common/mom/ pp,pmax,dcut
	   common/cplcns/cplas,scale,c0,c1,c2,
     u                   cdeltc,cdeltl,cfullc,cfulll,crm2
c
	   b0=11.d0-2.d0/3.d0*xnf
	   b1=102.d0-38.d0/3.d0*xnf
c
	   a1=31.d0/9.d0*ca-20.d0/9.d0*tf*xnf
	   a2=(4343.d0/162.d0+4.d0*pi**2-pi**4/4.d0+
     u         22.d0/3.d0*zeta3)*ca**2-
     u        (1798.d0/81.d0+56.d0/3.d0*zeta3)*ca*tf*xnf-
     u        (55.d0/3.d0-16.d0*zeta3)*cf*tf*xnf+
     u        (20.d0/9.d0*tf*xnf)**2
c
	   pm=p
	   xkm=xk
	   cnspot=-4.d0/3.d0*4.d0*pi
c
	      if (npot.eq.1) then
		 c0=1.d0
		 c1=0.d0
		 c2=0.d0
	      else if (npot.eq.3) then
		 c0=1.d0+alphas/(4.d0*pi)*a1
		 c1=alphas/(4.d0*pi)*b0
		 c2=0
	      else if (npot.eq.4) then
		 c0=1.d0+alphas/(4.d0*pi)*a1+(alphas/(4.d0*pi))**2*a2
		 c1=alphas/(4.d0*pi)*b0+
     u             (alphas/(4.d0*pi))**2*(b1+2.d0*b0*a1)
		 c2=(alphas/(4.d0*pi))**2*b0**2
	      else if (npot.eq.5) then
              else
		 write (*,*) ' Potential not implemented! Stop. 4'
		 stop
	      endif
	      phiint=cnspot*alphas
c
c Pure Coulomb in first order only:
	      xkpln1st=-(xk/p)**2*(1.d0+(xk**2+p**2)/(2.d0*xk*p)*
     u                  (log(abs(p-xk))-log(p+xk)))
		 vhhat=phiint*c0*xkpln1st
c
	end
c
c
!!! FB:
! 	real*8 function gtpcor(topp,etot)
! 	real*8 topp,etot,
!      u         tmass,tgamma,zmass,alphas,alamb5,
!      u         wmass,wgamma,bmass,GFERMI
!       COMMON/PHCONS/TMASS,TGAMMA,ZMASS,ALPHAS,ALAMB5,
!      $ WMASS,WGAMMA,BMASS,GFERMI
! 	gtpcor=1.d0
! 	end
!!! /FB
c
c
c --- Routines for solving linear equations and matrix inversion (complex) ---
c
	subroutine saeax(pp, w1, bb, vec, a1, n)
c
	   implicit none
	   complex*16 vhatax,vhhat
	   real*8
     u        tmass,tgamma,zmass,alphas,alamb5,
     u        wmass,wgamma,bmass,GFERMI,
     u        pi, energy, vzero, eps,
     u        d, d1, pp, w1, gtpcor,
     u        xp,xpmax,dcut,kincom,kincoa,kincov
	   complex*16 aax, a1, bb, vec, ff, kk, cw, svw, g0, g0c
	   integer i, j, npot, n, nmax, indx,kinflg,gcflg,vflag
	   parameter (nmax=400)
	   dimension bb(nmax),vec(nmax),ff(nmax,nmax),kk(nmax,nmax),
     u               pp(nmax),w1(nmax),indx(nmax),cw(nmax),a1(nmax)
c
      COMMON/PHCONS/TMASS,TGAMMA,ZMASS,ALPHAS,ALAMB5,
     $ WMASS,WGAMMA,BMASS,GFERMI
	   common/ovalco/ pi, energy, vzero, eps, npot
	   common/mom/ xp,xpmax,dcut
	   common/g0inf/kincom,kincoa,kincov,kinflg,gcflg,vflag
c
	   external aax, vhatax, gtpcor, g0, g0c, vhhat
c
	   do 10 i=1,n*2/3
	      cw(i) = w1(i) / (4.d0*pi**2) * g0c(pp(i))
c	      cw(i) = w1(i) / (4.d0*pi**2 *
c     u                (cmplx(energy-vzero, tgamma*
c     u                 gtpcor(pp(i),2.d0*tmass+energy),
c     u                    kind=kind(0d0))-pp(i)**2/tmass))
10	   continue
	   do 20 i=n*2/3+1,n
	      cw(i) = w1(i) / (4.d0*pi**2) * g0c(pp(i)) * pp(i)**2
c	      cw(i) = w1(i) / (4.d0*pi**2 *
c     u          (cmplx(energy-vzero, tgamma*
c     u           gtpcor(pp(i),2.d0*tmass+energy),kind=kind(0d0)) /
c     u           pp(i)**2 - 1.d0/tmass))
20	   continue
c
	   do 30 i=1,n
cc	      bb(i) = a1(i)
cvv
	      if (pp(i).lt.dcut.and.vflag.eq.1) then
c		 bb(i) = cmplx(1.d0+kincov*(pp(i)/tmass)**2,0.d0,
c     u                    kind=kind(0d0))
                 bb(i)=1.d0+kincov*
     u                       g0(pp(i))*(pp(i)/tmass)**2/g0c(pp(i))
	      else
		 bb(i) = (1.d0,0.d0)
	      endif
c
c Without extra kinematic corrections:
              vec(i)=(1.d0,0.d0)
c
	      svw = (0.d0,0.d0)
	      do 40 j=1,n
		 if (i.ne.j) then
		    ff(i,j) = - vhatax(pp(i),pp(j)) * cw(j)
		    kk(i,j) = - vhhat(pp(i),pp(j)) * cw(j)
		    svw = svw + ff(i,j)
		 endif
40	      continue
	      ff(i,i) = 1.d0 - a1(i) - svw
	      kk(i,i) = ff(i,i)
30	   continue
c
	   call zldcmp(ff, n, nmax, indx, d)
	   call zldcmp(kk, n, nmax, indx, d1)
	   call zlbksb(ff, n, nmax, indx, bb)
	   call zlbksb(kk, n, nmax, indx, vec)
c
	end
c
c
!!! FB:
!       SUBROUTINE ZLBKSB(A,N,NP,INDX,B)
! C complex version of lubksb
!       IMPLICIT NONE
!       INTEGER I, II, INDX, J, LL, N, NP
!       COMPLEX*16 A, B, SUM
!       DIMENSION A(NP,NP),INDX(N),B(N)
!       II=0
!       DO 12 I=1,N
!         LL=INDX(I)
!         SUM=B(LL)
!         B(LL)=B(I)
!         IF (II.NE.0)THEN
!           DO 11 J=II,I-1
!             SUM=SUM-A(I,J)*B(J)
! 11        CONTINUE
!         ELSE IF (SUM.NE.(0.D0,0.D0)) THEN
!           II=I
!         ENDIF
!         B(I)=SUM
! 12    CONTINUE
!       DO 14 I=N,1,-1
!         SUM=B(I)
!         IF(I.LT.N)THEN
!           DO 13 J=I+1,N
!             SUM=SUM-A(I,J)*B(J)
! 13        CONTINUE
!         ENDIF
!         B(I)=SUM/A(I,I)
! 14    CONTINUE
!       RETURN
!       END
! c
!       SUBROUTINE ZLDCMP(A,N,NP,INDX,D)
! C complex version of ludcmp
!       IMPLICIT NONE
!       INTEGER I, IMAX, INDX, J, K, N, NP, NMAX
!       REAL*8 AAMAX, D, TINY, VV
!       COMPLEX*16 A, DUM, SUM
!       PARAMETER (NMAX=400)
!       DIMENSION A(NP,NP), INDX(N), VV(NMAX)
! c
! 	tiny=1.d-5
! c
!       D=1.D0
!       DO 12 I=1,N
!         AAMAX=0.D0
!         DO 11 J=1,N
!           IF (ABS(A(I,J)).GT.AAMAX) AAMAX=ABS(A(I,J))
! 11      CONTINUE
!         IF (AAMAX.EQ.0.D0) PAUSE 'Singular matrix.'
!         VV(I)=1.D0/AAMAX
! 12    CONTINUE
!       DO 19 J=1,N
!         IF (J.GT.1) THEN
!           DO 14 I=1,J-1
!             SUM=A(I,J)
!             IF (I.GT.1)THEN
!               DO 13 K=1,I-1
!                 SUM=SUM-A(I,K)*A(K,J)
! 13            CONTINUE
!               A(I,J)=SUM
!             ENDIF
! 14        CONTINUE
!         ENDIF
!         AAMAX=0.D0
!         DO 16 I=J,N
!           SUM=A(I,J)
!           IF (J.GT.1)THEN
!             DO 15 K=1,J-1
!               SUM=SUM-A(I,K)*A(K,J)
! 15          CONTINUE
!             A(I,J)=SUM
!           ENDIF
!           DUM=VV(I)*ABS(SUM)
!           IF (ABS(DUM).GE.AAMAX) THEN
!             IMAX=I
!             AAMAX=DUM
!           ENDIF
! 16      CONTINUE
!         IF (J.NE.IMAX) THEN
!           DO 17 K=1,N
!             DUM=A(IMAX,K)
!             A(IMAX,K)=A(J,K)
!             A(J,K)=DUM
! 17        CONTINUE
!           D=-D
!           VV(IMAX)=VV(J)
!         ENDIF
!         INDX(J)=IMAX
!         IF (J.NE.N) THEN
!           IF (A(J,J).EQ.(0.D0,0.D0)) A(J,J)=cmplx(TINY, 0.d0,
!      u                    kind=kind(0d0))
!           DUM=1.D0/A(J,J)
!           DO 18 I=J+1,N
!             A(I,J)=A(I,J)*DUM
! 18        CONTINUE
!         ENDIF
! 19    CONTINUE
!       IF(A(N,N).EQ.(0.D0,0.D0)) A(N,N)=cmplx(TINY, 0.d0,
!      u                    kind=kind(0d0))
!       RETURN
!       END
! C
! C
! C *** TOOLS ***
! C
! C
! C     ******* ROUTINES FOR GAUSSIAN INTEGRATIONS
! C
! C
!       SUBROUTINE GAULEG(X1,X2,X,W,N)
! C
! C     Given the lower and upper limits of integration X1 and X2
! C     and given N, this routine returns arrays X(N) and W(N)
! C     containing the abscissas and weights of the Gauss-Legendre
! C     N-point quadrature formula
! C
!       IMPLICIT REAL*8 (A-H,O-Z)
!       REAL*8 X1,X2,X(N),W(N)
!       PARAMETER (EPS=3.D-14)
!       save
!       M=(N+1)/2
!       XM=0.5D0*(X2+X1)
!       XL=0.5D0*(X2-X1)
!       DO 12 I=1,M
!         Z=DCOS(3.141592653589793238D0*(I-.25D0)/(N+.5D0))
! 1       CONTINUE
!           P1=1.D0
!           P2=0.D0
!           DO 11 J=1,N
!             P3=P2
!             P2=P1
!             P1=((2.D0*J-1.D0)*Z*P2-(J-1.D0)*P3)/J
! 11        CONTINUE
!           PP=N*(Z*P1-P2)/(Z*Z-1.D0)
!           Z1=Z
!           Z=Z1-P1/PP
!         IF(DABS(Z-Z1).GT.EPS)GO TO 1
!         X(I)=XM-XL*Z
!         X(N+1-I)=XM+XL*Z
!         W(I)=2.D0*XL/((1.D0-Z*Z)*PP*PP)
!         W(N+1-I)=W(I)
! 12    CONTINUE
!       RETURN
!       END
! C
! C
!       DOUBLE PRECISION FUNCTION AD8GLE(F,A,B,EPS)
!       implicit double precision (a-h,o-z)
!       EXTERNAL F
!       DIMENSION W(12),X(12)
! c      SAVE W, X
!       SAVE
! C
! C     ******************************************************************
! C
! C     ADAPTIVE GAUSSIAN QUADRATURE.
! C
! C     AD8GLE IS SET EQUAL TO THE APPROXIMATE VALUE OF THE INTEGRAL OF
! C     THE FUNCTION F OVER THE INTERVAL (A,B), WITH ACCURACY PARAMETER
! C     EPS.
! C
! C     ******************************************************************
! C
!       DATA W / 0.10122 85362 90376 25915 25313 543D0,
!      $         0.22238 10344 53374 47054 43559 944D0,
!      $         0.31370 66458 77887 28733 79622 020D0,
!      $         0.36268 37833 78361 98296 51504 493D0,
!      $         0.27152 45941 17540 94851 78057 246D-1,
!      $         0.62253 52393 86478 92862 84383 699D-1,
!      $         0.95158 51168 24927 84809 92510 760D-1,
!      $         0.12462 89712 55533 87205 24762 822D0,
!      $         0.14959 59888 16576 73208 15017 305D0,
!      $         0.16915 65193 95002 53818 93120 790D0,
!      $         0.18260 34150 44923 58886 67636 680D0,
!      $         0.18945 06104 55068 49628 53967 232D0/
! C
!       DATA X / 0.96028 98564 97536 23168 35608 686D0,
!      $         0.79666 64774 13626 73959 15539 365D0,
!      $         0.52553 24099 16328 98581 77390 492D0,
!      $         0.18343 46424 95649 80493 94761 424D0,
!      $         0.98940 09349 91649 93259 61541 735D0,
!      $         0.94457 50230 73232 57607 79884 155D0,
!      $         0.86563 12023 87831 74388 04678 977D0,
!      $         0.75540 44083 55003 03389 51011 948D0,
!      $         0.61787 62444 02643 74844 66717 640D0,
!      $         0.45801 67776 57227 38634 24194 430D0,
!      $         0.28160 35507 79258 91323 04605 015D0,
!      $         0.95012 50983 76374 40185 31933 543D-1/
! C
! C     ******************************************************************
! C
!       GAUSS=0.0D0
!       AD8GLE=GAUSS
!       IF(B.EQ.A) RETURN
!       CONST=EPS/(B-A)
!       BB=A
! C
! C  COMPUTATIONAL LOOP.
!     1 AA=BB
!       BB=B
!     2    C1=0.5D0*(BB+AA)
!          C2=0.5D0*(BB-AA)
!          S8=0.0D0
!          DO 3 I=1,4
!             U=C2*X(I)
!             S8=S8+W(I)*(F(C1+U)+F(C1-U))
!     3    CONTINUE
!          S8=C2*S8
!          S16=0.0D0
!          DO 4 I=5,12
!             U=C2*X(I)
!             S16=S16+W(I)*(F(C1+U)+F(C1-U))
!     4    CONTINUE
!          S16=C2*S16
!          IF( ABS(S16-S8) .LE. EPS*(abs(s8)+ABS(S16))*0.5D0 ) GO TO 5
!          BB=C1
!          IF( 1.D0+ABS(CONST*C2) .NE. 1.D0) GO TO 2
!       AD8GLE=0.0D0
!       write(*,*)'too high accuracy required in function ad8gle!'
!       RETURN
!     5 GAUSS=GAUSS+S16
!       IF(BB.NE.B) GO TO 1
!       AD8GLE=GAUSS
!       RETURN
!       END
! C
! C
!       DOUBLE PRECISION FUNCTION ADGLG1(F,A,B,EPS)
!       IMPLICIT REAL*8 (A-H,O-Z)
!       EXTERNAL F,AD8GLE,adqua
!       DIMENSION W(6),X(6),xx(6)
! c      SAVE W, XX, NUM
!       SAVE
! C
! C     ******************************************************************
! C
! C     ADAPTIVE GAUSSIAN QUADRATURE.
! C     For x->b   f(x) = O (ln^k (b-x) )
! C     A - lower limit, B - upper limit (integrable singularity)
! C     AD8GLE IS SET EQUAL TO THE APPROXIMATE VALUE OF THE INTEGRAL OF
! C     THE FUNCTION F OVER THE INTERVAL (A,B), WITH ACCURACY PARAMETER
! C     EPS.
! C
! C     ******************************************************************
!       DATA W / 4.58964 673950d-1,
!      $         4.17000 830772d-1,
!      $         1.13373 382074d-1,
!      $         1.03991 974531d-2,
!      $         2.61017 202815d-4,
!      $         8.98547 906430d-7/
! C
!       DATA X / 0.22284 66041 79d0,
!      $         1.18893 21016 73d0,
!      $         2.99273 63260 59d0,
!      $         5.77514 35691 05d0,
!      $         9.83746 74183 83d0,
!      $        15.98287 39806 02d0/
!       DATA NUM/0/
!       IF(NUM.eq.0d0) then
!       do 1 ix=1,6
!   1   xx(ix)= EXP(-x(ix))
!       ENDIF
!       num=num+1
!       sum=0d0
!       c=b-a
!       sum6=0d0
!       do 10 in=1,6
!  10   sum6= sum6+ w(in)*f(b-c*xx(in))
!       sum6=sum6*c
!       a1=a
!  15   a2= (a1+b)/2
!       c=b-a2
!       sumn=0d0
!       do 20 in=1,6
!  20   sumn= sumn+ w(in)*f(b-c*xx(in)) !!! FB: f(b) = NaN !
!       sumn=sumn*c
! ctt
! c      call adqua(a1,a2,f,sum1,eps)
! c      sum1=sum1+sum
!       sum1=AD8GLE(F,A1,A2,eps)+sum
!       IF(ABS( (sum+sum6)/(sum1+sumn)-1d0 ).lt.EPS) THEN
! ctt
! c      call adqua(a,a2,f,sum2,eps)
!          sum2=AD8GLE(F,A,A2,eps)
!          IF(ABS( (sum2+sumn)/(sum1+sumn)-1d0 ).gt.EPS) THEN
!             sum=sum2
!             a1=a2
!             sum6=sumn
!             goto 15
!          ENDIF
!          ADGLG1= SUM1+SUMN
!          RETURN
!       ELSE
!          sum=sum1
!          a1=a2
!          sum6=sumn
!          goto 15
!       ENDIF
!       END
! C
!       DOUBLE PRECISION FUNCTION ADGLG2(F,A,B,EPS)
!       IMPLICIT REAL*8 (A-H,O-Z)
!       EXTERNAL F,AD8GLE
!       DIMENSION W(6),X(6),xx(6)
! c      SAVE W,XX,NUM
!       SAVE
! C
! C     ******************************************************************
! C
! C     ADAPTIVE GAUSSIAN QUADRATURE.
! C     For x->A   f(x) = O (ln^k (x-a) )
! C     A - lower limit  (integrable singularity), B - upper limit
! C     AD8GLE IS SET EQUAL TO THE APPROXIMATE VALUE OF THE INTEGRAL OF
! C     THE FUNCTION F OVER THE INTERVAL (A,B), WITH ACCURACY PARAMETER
! C     EPS.
! C
! C     ******************************************************************
!       DATA W / 4.58964 673950d-1,
!      $         4.17000 830772d-1,
!      $         1.13373 382074d-1,
!      $         1.03991 974531d-2,
!      $         2.61017 202815d-4,
!      $         8.98547 906430d-7/
! C
!       DATA X / 0.22284 66041 79d0,
!      $         1.18893 21016 73d0,
!      $         2.99273 63260 59d0,
!      $         5.77514 35691 05d0,
!      $         9.83746 74183 83d0,
!      $        15.98287 39806 02d0/
!       DATA NUM/0/
!       IF(NUM.eq.0d0) then
!       do 1 ix=1,6
!   1   xx(ix)= EXP(-x(ix))
!       ENDIF
!       num=num+1
!       sum=0d0
!       c=b-a
!       sum6=0d0
!       do 10 in=1,6
!  10   sum6= sum6+ w(in)*f(A+c*xx(in))
!       sum6=sum6*c
!       b1=b
!  15   b2= (a+b1)/2
!       c=b2-a
!       sumn=0d0
!       do 20 in=1,6
!  20   sumn= sumn+ w(in)*f(a+c*xx(in)) !!! FB: f(a) = NaN !
!       sumn=sumn*c
!       sum1=AD8GLE(F,b2,b1,eps)+sum
!       IF(ABS( (sum+sum6)/(sum1+sumn)-1d0 ).lt.EPS) THEN
!          sum2=AD8GLE(F,b2,b,eps)
!          IF(ABS( (sum2+sumn)/(sum1+sumn)-1d0 ).gt.EPS) THEN
!             sum=sum2
!             b1=b2
!             sum6=sumn
!             goto 15
!          ENDIF
!          ADGLG2= SUM1+SUMN
!          RETURN
!       ELSE
!          sum=sum1
!          b1=b2
!          sum6=sumn
!          goto 15
!       ENDIF
!       END
! C
! C
! C------------------------------------------------------------------
! C INTEGRATION ROUTINE ADQUA written by M. Jezabek            ------
! C------------------------------------------------------------------
! C
!       SUBROUTINE ADQUA(XL,XU,F,Y,ACC)
! C
! C     ADAPTIVE GAUSS-LEGENDRE + SIMPSON'S RULE QUADRATURE
! C     XL - LOWER LIMIT, XU - UPPER LIMIT, F - FUNCTION TO INTEGRATE
! C     Y - INTEGRAL
! C     ACC - ACCURACY (IF .LE. 0.  ACC=1.D-6)
! c     ****** new constants,  1 error removed, Oct '92
! C
! C     CALLS: SIMPSA
! C
! C     PARAMETERS: NSUB > NO OF SUBDIVISION LEVELS IN GAUSS INTEGRATION
! C          100*2**IMAX > NO OF POINTS IN SIMPSON INTEGRATION
! C
!       IMPLICIT REAL*8 (A-H,O-Z)
!       EXTERNAL F
!       DIMENSION VAL(25,2), BOUND(25,2,2), LEV(25),SING(25,3)
!       DIMENSION W8(4),X8(4)
!       DATA W8
!      $/0.101228536290376D0, 0.222381034453374D0, 0.313706645877887D0,
!      $ 0.362683783378362D0/
!       DATA X8
!      $/0.960289856497536D0, 0.796666477413627D0, 0.525532409916329D0,
!      $ 0.183434642495650D0/
!       save
! C
!       IF(ACC.LE.0.D0) ACC=1.D-6
!       NSUB=24
!       NSG=25
!       NSC=0
!       A=XL
!       B=XU
!       C1=0.5d0*(A+B)
!       C2=C1-A
!       S8=0d0
!       DO 1 I=1,4
!       U=X8(I)*C2
!     1 S8=S8+W8(I)*(F(C1+U)+F(C1-U))
!       S8=S8*C2
!       XM=(XL+XU)/2.d0
!       BOUND(1,1,1)=XL
!       BOUND(1,1,2)=XM
!       BOUND(1,2,1)=XM
!       BOUND(1,2,2)=XU
!       NC=1
!       DO 3 IX=1,2
!       A=BOUND(NC,IX,1)
!       B=BOUND(NC,IX,2)
!       C1=0.5d0*(A+B)
!       C2=C1-A
!       VAL(NC,IX)=0.d0
!       DO 2 I=1,4
!       U=X8(I)*C2
!     2 VAL(NC,IX)=VAL(NC,IX)+W8(I)*(F(C1+U)+F(C1-U))
!     3 VAL(NC,IX)=VAL(NC,IX)*C2
!       S16=VAL(NC,1)+VAL(NC,2)
!       IF(DABS(S8-S16).GT.ACC*DABS(S16)) GOTO 4
!       Y=S16
!       RETURN
!     4 DO 5 I=1,NSUB
!     5 LEV(I)=0
!       NC1= NC+1
!    11 XM=(BOUND(NC,1,1)+BOUND(NC,1,2))/2.d0
!       BOUND(NC1,1,1)=BOUND(NC,1,1)
!       BOUND(NC1,1,2)=XM
!       BOUND(NC1,2,1)=XM
!       BOUND(NC1,2,2)=BOUND(NC,1,2)
!       DO 13 IX=1,2
!       A=BOUND(NC1,IX,1)
!       B=BOUND(NC1,IX,2)
!       C1=0.5d0*(A+B)
!       C2=C1-A
!       VAL(NC1,IX)=0.d0
!       DO 12 I=1,4
!       U=X8(I)*C2
!    12 VAL(NC1,IX)=VAL(NC1,IX)+W8(I)*(F(C1+U)+F(C1-U))
!    13 VAL(NC1,IX)=VAL(NC1,IX)*C2
!       S16=VAL(NC1,1)+VAL(NC1,2)
!       S8=VAL(NC,1)
!       IF(DABS(S8-S16).LE.ACC*DABS(S16)) GOTO 20
!       NC=NC1
!       NC1= NC+1
!       IF(NC1.LE.NSUB) GOTO 11
! C     NC=NSUB   USE SIMPSON'S RULE
!       NSC=NSC+1
!       IF(NSC.LE.NSG) GOTO 15
!       WRITE(*,911)
!   911 FORMAT(1X,'ADQUA: TOO MANY SINGULARITIES')
!       STOP
!    15 SING(NSC,1)=BOUND(NC,1,1)
!       SING(NSC,2)=BOUND(NC,2,2)
!       SING(NSC,3)=S16
!       S16=0.d0
!       NC=NC-1
!    20 VAL(NC,1)= S16
!   121 LEV(NC)=1
!    21 XM=(BOUND(NC,2,1)+BOUND(NC,2,2))/2.d0
!       BOUND(NC1,1,1)=BOUND(NC,2,1)
!       BOUND(NC1,1,2)=XM
!       BOUND(NC1,2,1)=XM
!       BOUND(NC1,2,2)=BOUND(NC,2,2)
!       DO 23 IX=1,2
!       A=BOUND(NC1,IX,1)
!       B=BOUND(NC1,IX,2)
!       C1=0.5d0*(A+B)
!       C2=C1-A
!       VAL(NC1,IX)=0.d0
!       DO 22 I=1,4
!       U=X8(I)*C2
!    22 VAL(NC1,IX)=VAL(NC1,IX)+W8(I)*(F(C1+U)+F(C1-U))
!    23 VAL(NC1,IX)=VAL(NC1,IX)*C2
!       S16=VAL(NC1,1)+VAL(NC1,2)
!       S8=VAL(NC,2)
!       IF(DABS(S8-S16).LE.ACC*DABS(S16)) GOTO 40
!       NC=NC+1
!       NC1=NC+1
!       IF(NC1.LE.NSUB) GOTO 11
! C     NC=NSUB   USE SIMPSON'S RULE
!       NSC=NSC+1
!       IF(NSC.LE.NSG) GOTO 35
!       WRITE(*,911)
!       STOP
!    35 SING(NSC,1)=BOUND(NC,1,1)
!       SING(NSC,2)=BOUND(NC,2,2)
!       SING(NSC,3)=S16
!       S16=0.d0
!       NC=NC-1
!    40 VAL(NC,2)= S16
!    45 IF(NC.GT.1) GOTO 50
!       Y1=VAL(1,1)+VAL(1,2)
!       GOTO 100
!    50 NC0=NC-1
!       IF(LEV(NC0).EQ.0) IX=1
!       IF(LEV(NC0).EQ.1) IX=2
!       LEV(NC)=0
!       NC1=NC
!       VAL(NC0,IX)=VAL(NC,1)+VAL(NC,2)
!       NC=NC0
!       IF(IX.EQ.1) GOTO 121
!       GOTO 45
!   100 CONTINUE
!       IF(NSC.GT.0) GOTO 101
!       Y=Y1
!       RETURN
!   101 FSUM=0.d0
!       DO 102 IK=1,NSC
!   102 FSUM=FSUM+DABS(SING(IK,3))
!       ACCR=ACC*DMAX1(FSUM,DABS(Y1))/FSUM/10.d0
!       DO 104 IK=1,NSC
!   104 CALL SIMPSA(SING(IK,1),SING(IK,2),F,SING(IK,3),ACCR)
!       DO 106 IK=1,NSC
!   106 Y1=Y1+SING(IK,3)
!       Y=Y1
!       RETURN
!       END
! C
!       SUBROUTINE SIMPSA(A,B,F,F0,ACC)
! C     SIMPSON'S ADAPTIVE QUADRATURE
!       IMPLICIT REAL*8 (A-H,O-Z)
!       save
!       EXTERNAL F
!       IMAX=5
!       N0=100
!       H=(B-A)/N0
!       N02=N0/2
!       S2=0.d0
!       IC=1
!       S0=F(A)+F(B)
!       DO 5 K=1,N02
!     5 S2=S2+F(A+2.d0*K*H)
!     7 S1=0.d0
!       DO 10 K=1,N02
!    10 S1=S1+F(A+(2.d0*K-1.d0)*H)
!       Y=H/3.d0*(S0+4.d0*S1+2.d0*S2)
!       IF(DABS(F0/Y-1.d0).GT.ACC) GOTO 20
!       RETURN
!    20 N02=N0
!       N0=2*N0
!       S2=S1+S2
!       H=H/2.d0
!       IF(IC.GT.IMAX) GOTO 30
!       F0=Y
!       IC=IC+1
!       GOTO 7
!    30 ACC0=DABS(Y/F0-1.d0)
!       WRITE(*,900) A,B,ACC0
!       STOP
!   900 FORMAT(1H ,'SIMPSA: TOO HIGH ACCURACY REQUIRED'/
!      /1X,   29HSINGULARITY IN THE INTERVAL  ,D20.12,1X,D20.12/
!      /1X,   29HACCURACY ACHIEVED            ,D20.12)
!       END
! C
! C
! C  ******* matrix-inversion-routines
! C
!       SUBROUTINE LUDCMP(A,N,NP,INDX,D)
!       IMPLICIT REAL*8(A-H,O-Z)
!       PARAMETER (NMAX=100,TINY=1.0E-20)
!       DIMENSION A(NP,NP),INDX(N),VV(NMAX)
!       D=1.
!       DO 12 I=1,N
!         AAMAX=0.
!         DO 11 J=1,N
!           IF (ABS(A(I,J)).GT.AAMAX) AAMAX=ABS(A(I,J))
! 11      CONTINUE
!         IF (AAMAX.EQ.0.) PAUSE 'Singular matrix.'
!         VV(I)=1./AAMAX
! 12    CONTINUE
!       DO 19 J=1,N
!         IF (J.GT.1) THEN
!           DO 14 I=1,J-1
!             SUM=A(I,J)
!             IF (I.GT.1)THEN
!               DO 13 K=1,I-1
!                 SUM=SUM-A(I,K)*A(K,J)
! 13            CONTINUE
!               A(I,J)=SUM
!             ENDIF
! 14        CONTINUE
!         ENDIF
!         AAMAX=0.
!         DO 16 I=J,N
!           SUM=A(I,J)
!           IF (J.GT.1)THEN
!             DO 15 K=1,J-1
!               SUM=SUM-A(I,K)*A(K,J)
! 15          CONTINUE
!             A(I,J)=SUM
!           ENDIF
!           DUM=VV(I)*ABS(SUM)
!           IF (DUM.GE.AAMAX) THEN
!             IMAX=I
!             AAMAX=DUM
!           ENDIF
! 16      CONTINUE
!         IF (J.NE.IMAX)THEN
!           DO 17 K=1,N
!             DUM=A(IMAX,K)
!             A(IMAX,K)=A(J,K)
!             A(J,K)=DUM
! 17        CONTINUE
!           D=-D
!           VV(IMAX)=VV(J)
!         ENDIF
!         INDX(J)=IMAX
!         IF(J.NE.N)THEN
!           IF(A(J,J).EQ.0.)A(J,J)=TINY
!           DUM=1./A(J,J)
!           DO 18 I=J+1,N
!             A(I,J)=A(I,J)*DUM
! 18        CONTINUE
!         ENDIF
! 19    CONTINUE
!       IF(A(N,N).EQ.0.)A(N,N)=TINY
!       RETURN
!       END
! c
!       SUBROUTINE LUBKSB(A,N,NP,INDX,B)
!       IMPLICIT REAL*8(A-H,O-Z)
!       DIMENSION A(NP,NP),INDX(N),B(N)
!       II=0
!       DO 12 I=1,N
!         LL=INDX(I)
!         SUM=B(LL)
!         B(LL)=B(I)
!         IF (II.NE.0)THEN
!           DO 11 J=II,I-1
!             SUM=SUM-A(I,J)*B(J)
! 11        CONTINUE
!         ELSE IF (SUM.NE.0.) THEN
!           II=I
!         ENDIF
!         B(I)=SUM
! 12    CONTINUE
!       DO 14 I=N,1,-1
!         SUM=B(I)
!         IF(I.LT.N)THEN
!           DO 13 J=I+1,N
!             SUM=SUM-A(I,J)*B(J)
! 13        CONTINUE
!         ENDIF
!         B(I)=SUM/A(I,I)
! 14    CONTINUE
!       RETURN
!       END
! C
! C
! C     *******  RANDOM NUMBER GENERATORS
! C
! C
!       FUNCTION RANF(DUMMY)
! C
! C   RANDOM NUMBER FUNCTION TAKEN FROM KNUTH
! C   (SEMINUMERICAL ALGORITHMS).
! C   METHOD IS X(N)=MOD(X(N-55)-X(N-24),1/FMODUL)
! C   NO PROVISION YET FOR CONTROL OVER THE SEED NUMBER.
! C
! C   RANF GIVES ONE RANDOM NUMBER BETWEEN 0 AND 1.
! C   IRN55 GENERATES 55 RANDOM NUMBERS BETWEEN 0 AND 1/FMODUL.
! C   IN55  INITIALIZES THE 55 NUMBERS AND WARMS UP THE SEQUENCE.
! C
!       PARAMETER (FMODUL=1.E-09)
!       SAVE /CIRN55/
!       COMMON /CIRN55/NCALL,MCALL,IA(55)
!       INTEGER IA
!       CALL RANDAT
!       IF( NCALL.EQ.0 ) THEN
!           CALL IN55 ( IA,234612947 )
!           MCALL = 55
!           NCALL = 1
!       ENDIF
!       IF ( MCALL.EQ.0 ) THEN
!           CALL IRN55(IA)
!           MCALL=55
!       ENDIF
!       RANF=IA(MCALL)*FMODUL
!       MCALL=MCALL-1
!       RETURN
!       END
! C
!       SUBROUTINE RANDAT
! C
! C  INITIALISES THE NUMBER NCALL TO 0 TO FLAG THE FIRST CALL
! C  OF THE RANDOM NUMBER GENERATOR
! C
! C      SAVE /CIRN55/
! C      SAVE FIRST
!       SAVE
!       COMMON /CIRN55/NCALL,MCALL,IA(55)
!       INTEGER IA
!       LOGICAL FIRST
!       DATA FIRST /.TRUE./
!       IF(FIRST)THEN
!          FIRST=.FALSE.
!          NCALL=0
!       ENDIF
!       RETURN
!       END
! C
!       SUBROUTINE IN55(IA,IX)
!       PARAMETER (MODULO=1000000000)
!       INTEGER IA(55)
! C
!       IA(55)=IX
!       J=IX
!       K=1
!       DO 10 I=1,54
!          II=MOD(21*I,55)
!          IA(II)=K
!          K=J-K
!          IF(K.LT.0)K=K+MODULO
!          J=IA(II)
! 10    CONTINUE
!       DO 20 I=1,10
!          CALL IRN55(IA)
! 20    CONTINUE
!       RETURN
!       END
! C
!       SUBROUTINE IRN55(IA)
!       PARAMETER (MODULO=1000000000)
!       INTEGER IA(55)
!       DO 10 I=1,24
!          J=IA(I)-IA(I+31)
!          IF(J.LT.0)J=J+MODULO
!          IA(I)=J
! 10    CONTINUE
!       DO 20 I=25,55
!          J=IA(I)-IA(I-24)
!          IF(J.LT.0)J=J+MODULO
!          IA(I)=J
! 20    CONTINUE
!       RETURN
!       END
! C
! C
!       FUNCTION RAN2(IDUM)
! C     *******************
!       REAL RDM(31)
!       DATA  IWARM/0/
! C
!       IF (IDUM.LT.0.OR.IWARM.EQ.0) THEN
! C INITIALIZATION OR REINITIALISATION
!       IWARM=1
!       IA1=         1279
!       IC1=       351762
!       M1=       1664557
!       IA2=         2011
!       IC2=       221592
!       M2=       1048583
!       IA3=        15091
!       IC3=         6171
!       M3=         29201
!       IX1=MOD(-IDUM,M1)
!       IX1=MOD(IA1*IX1+IC1,M1)
!       IX2=MOD(IX1,M2)
!       IX1=MOD(IA1*IX1+IC1,M1)
!       IX3=MOD(IX1,M3)
!       RM1=1./FLOAT(M1)
!       RM2=1./FLOAT(M2)
!       DO 10 J=1,31
!       IX1=MOD(IA1*IX1+IC1,M1)
!       IX2=MOD(IA2*IX2+IC2,M2)
! 10    RDM(J)=(FLOAT(IX1)+FLOAT(IX2)*RM2)*RM1
!       ENDIF
! C
! C GENERATE NEXT NUMBER IN SEQUENCE
!       IF(IWARM.EQ.0) GOTO 901
!       IX1=MOD(IA1*IX1+IC1,M1)
!       IX2=MOD(IA2*IX2+IC2,M2)
!       IX3=MOD(IA3*IX3+IC3,M3)
!       J=1+(31*IX3)/M3
!       RAN2=RDM(J)
!       RDM(J)=(FLOAT(IX1)+FLOAT(IX2)*RM2)*RM1
!       RETURN
! 901   PRINT 9010
! 9010  FORMAT('   RAN2: LACK OF ITINIALISATION')
!       STOP
!       END
! C
! C
! C     *******    SPECIAL FUNCTIONS
! C
! C
!       DOUBLE PRECISION FUNCTION DILOG(X)
! C
! C     SPENCE'S DILOGARITHM IN DOUBLE PRECISION
! C
!       IMPLICIT REAL*8 (A-H,O-Z)
!       Z=-1.644934066848226
!       IF(X .LT.-1.0) GO TO 1
!       IF(X .LE. 0.5) GO TO 2
!       IF(X .EQ. 1.0) GO TO 3
!       IF(X .LE. 2.0) GO TO 4
!       Z=3.289868133696453
!     1 T=1.0/X
!       S=-0.5
!       Z=Z-0.5*DLOG(DABS(X))**2
!       GO TO 5
!     2 T=X
!       S=0.5
!       Z=0.
!       GO TO 5
!     3 DILOG=1.644934066848226
!       RETURN
!     4 T=1.0-X
!       S=-0.5
!       Z=1.644934066848226-DLOG(X)*DLOG(DABS(T))
!     5 Y=2.666666666666667*T+0.666666666666667
!       B=      0.00000 00000 00001
!       A=Y*B  +0.00000 00000 00004
!       B=Y*A-B+0.00000 00000 00011
!       A=Y*B-A+0.00000 00000 00037
!       B=Y*A-B+0.00000 00000 00121
!       A=Y*B-A+0.00000 00000 00398
!       B=Y*A-B+0.00000 00000 01312
!       A=Y*B-A+0.00000 00000 04342
!       B=Y*A-B+0.00000 00000 14437
!       A=Y*B-A+0.00000 00000 48274
!       B=Y*A-B+0.00000 00001 62421
!       A=Y*B-A+0.00000 00005 50291
!       B=Y*A-B+0.00000 00018 79117
!       A=Y*B-A+0.00000 00064 74338
!       B=Y*A-B+0.00000 00225 36705
!       A=Y*B-A+0.00000 00793 87055
!       B=Y*A-B+0.00000 02835 75385
!       A=Y*B-A+0.00000 10299 04264
!       B=Y*A-B+0.00000 38163 29463
!       A=Y*B-A+0.00001 44963 00557
!       B=Y*A-B+0.00005 68178 22718
!       A=Y*B-A+0.00023 20021 96094
!       B=Y*A-B+0.00100 16274 96164
!       A=Y*B-A+0.00468 63619 59447
!       B=Y*A-B+0.02487 93229 24228
!       A=Y*B-A+0.16607 30329 27855
!       A=Y*A-B+1.93506 43008 69969
!       DILOG=S*T*(A-B)+Z
!       RETURN
!       END
! C
! c Everything for hypergeometric function F_{2,1},
! c  taken from Numerical Recipes.
! c
!       FUNCTION hypgeo(a,b,c,z)
!       implicit none
!       COMPLEX*16 hypgeo,a,b,c,z,hp
!       REAL*8 EPS
! cttt
! c      PARAMETER (EPS=1.d-6)
!       PARAMETER (EPS=1.d-8)
!       INTEGER kmax,nbad,nok
!       COMPLEX*16 z0,dz,aa,bb,cc,y(2)
! cu    needs bsstep,hypser,odeint,mmid,hypdrv
!       COMMON /hypg/ aa,bb,cc,z0,dz
!       COMMON /path1/ kmax
!       kmax=0
!       if (real(z)**2+aimag(z)**2.le.0.25d0) then
!         call hypser(a,b,c,z,hp,y(2))
!         hypgeo=hp
!         return
!       else if (real(z).lt.0.d0) then
!         z0=cmplx(-0.5d0,0.d0,kind=kind(0d0))
!       else if (real(z).le.1.d0) then
!         z0=cmplx(0.5d0,0.d0,kind=kind(0d0))
!       else
!         z0=cmplx(0.d0,sign(0.5d0,aimag(z)),kind=kind(0d0))
!       endif
!       aa=a
!       bb=b
!       cc=c
!       dz=z-z0
!       call hypser(aa,bb,cc,z0,y(1),y(2))
!       call odeint(y,4,0.d0,1.d0,EPS,.1d0,.00001d0,nok,nbad)
!       hypgeo=y(1)
!       return
!       END
! c
!       SUBROUTINE odeint(ystart,nvar,x1,x2,eps,h1,hmin,nok,nbad)
!       implicit none
!       INTEGER nbad,nok,nvar,KMAXX,MAXSTP,NMAX
!       REAL*8 eps,h1,hmin,x1,x2,ystart(nvar),TINY
!       PARAMETER (MAXSTP=10000,NMAX=50,KMAXX=200,TINY=1.d-30)
!       INTEGER i,kmax,kount,nstp
!       REAL*8 dxsav,h,hdid,hnext,x,xsav,dydx(NMAX),xp(KMAXX),y(NMAX),
!      *yp(NMAX,KMAXX),yscal(NMAX)
!       COMMON /path/ dxsav,xp,yp,kount
!       COMMON /path1/ kmax
!       x=x1
!       h=sign(h1,x2-x1)
!       nok=0
!       nbad=0
!       kount=0
!       do 11 i=1,nvar
!         y(i)=ystart(i)
! 11    continue
!       if (kmax.gt.0) xsav=x-2.d0*dxsav
!       do 16 nstp=1,MAXSTP
!         call hypdrv(x,y,dydx)
!         do 12 i=1,nvar
!           yscal(i)=abs(y(i))+abs(h*dydx(i))+TINY
! 12      continue
!         if(kmax.gt.0)then
!           if(abs(x-xsav).gt.abs(dxsav)) then
!             if(kount.lt.kmax-1)then
!               kount=kount+1
!               xp(kount)=x
!               do 13 i=1,nvar
!                 yp(i,kount)=y(i)
! 13            continue
!               xsav=x
!             endif
!           endif
!         endif
!         if((x+h-x2)*(x+h-x1).gt.0.d0) h=x2-x
!         call bsstep(y,dydx,nvar,x,h,eps,yscal,hdid,hnext)
!         if(hdid.eq.h)then
!           nok=nok+1
!         else
!           nbad=nbad+1
!         endif
!         if((x-x2)*(x2-x1).ge.0.d0)then
!           do 14 i=1,nvar
!             ystart(i)=y(i)
! 14        continue
!           if(kmax.ne.0)then
!             kount=kount+1
!             xp(kount)=x
!             do 15 i=1,nvar
!               yp(i,kount)=y(i)
! 15          continue
!           endif
!           return
!         endif
!         if(abs(hnext).lt.hmin) pause
!      *'stepsize smaller than minimum in odeint'
!         h=hnext
! 16    continue
!       pause 'too many steps in odeint'
!       return
!       END
! c
!       SUBROUTINE bsstep(y,dydx,nv,x,htry,eps,yscal,hdid,hnext)
!       implicit none
!       INTEGER nv,NMAX,KMAXX,IMAX
!       REAL*8 eps,hdid,hnext,htry,x,dydx(nv),y(nv),yscal(nv),
!      *SAFE1,SAFE2,REDMAX,REDMIN,TINY,SCALMX
!       PARAMETER (NMAX=50,KMAXX=8,IMAX=KMAXX+1,SAFE1=.25d0,SAFE2=.7d0,
!      *REDMAX=1.d-5,REDMIN=.7d0,TINY=1.d-30,SCALMX=.1d0)
!       INTEGER i,iq,k,kk,km,kmax,kopt,nseq(IMAX)
!       REAL*8 eps1,epsold,errmax,fact,h,red,scale,work,wrkmin,xest,
!      *xnew,a(IMAX),alf(KMAXX,KMAXX),err(KMAXX),yerr(NMAX),ysav(NMAX),
!      *yseq(NMAX)
!       LOGICAL first,reduct
!       SAVE a,alf,epsold,first,kmax,kopt,nseq,xnew
!       DATA first/.true./,epsold/-1.d0/
!       DATA nseq /2,4,6,8,10,12,14,16,18/
!       if(eps.ne.epsold)then
!         hnext=-1.d29
!         xnew=-1.d29
!         eps1=SAFE1*eps
!         a(1)=nseq(1)+1
!         do 11 k=1,KMAXX
!           a(k+1)=a(k)+nseq(k+1)
! 11      continue
!         do 13 iq=2,KMAXX
!           do 12 k=1,iq-1
!             alf(k,iq)=eps1**((a(k+1)-a(iq+1))/((a(iq+1)-a(1)+1.d0)*
!      *(2.d0*k+1.d0)))
! 12        continue
! 13      continue
!         epsold=eps
!         do 14 kopt=2,KMAXX-1
!           if(a(kopt+1).gt.a(kopt)*alf(kopt-1,kopt))goto 1
! 14      continue
! 1       kmax=kopt
!       endif
!       h=htry
!       do 15 i=1,nv
!         ysav(i)=y(i)
! 15    continue
!       if(h.ne.hnext.or.x.ne.xnew)then
!         first=.true.
!         kopt=kmax
!       endif
!       reduct=.false.
! 2     do 17 k=1,kmax
!         xnew=x+h
!         if(xnew.eq.x)pause 'step size underflow in bsstep'
!         call mmid(ysav,dydx,nv,x,h,nseq(k),yseq)
!         xest=(h/nseq(k))**2
!         call pzext0(k,xest,yseq,y,yerr,nv)
!         if(k.ne.1)then
!           errmax=TINY
!           do 16 i=1,nv
!             errmax=max(errmax,abs(yerr(i)/yscal(i)))
! 16        continue
!           errmax=errmax/eps
!           km=k-1
!           err(km)=(errmax/SAFE1)**(1.d0/(2.d0*km+1.d0))
!         endif
!         if(k.ne.1.and.(k.ge.kopt-1.or.first))then
!           if(errmax.lt.1.)goto 4
!           if(k.eq.kmax.or.k.eq.kopt+1)then
!             red=SAFE2/err(km)
!             goto 3
!           else if(k.eq.kopt)then
!             if(alf(kopt-1,kopt).lt.err(km))then
!               red=1.d0/err(km)
!               goto 3
!             endif
!           else if(kopt.eq.kmax)then
!             if(alf(km,kmax-1).lt.err(km))then
!               red=alf(km,kmax-1)*SAFE2/err(km)
!               goto 3
!             endif
!           else if(alf(km,kopt).lt.err(km))then
!             red=alf(km,kopt-1)/err(km)
!             goto 3
!           endif
!         endif
! 17    continue
! 3     red=min(red,REDMIN)
!       red=max(red,REDMAX)
!       h=h*red
!       reduct=.true.
!       goto 2
! 4     x=xnew
!       hdid=h
!       first=.false.
!       wrkmin=1.d35
!       do 18 kk=1,km
!         fact=max(err(kk),SCALMX)
!         work=fact*a(kk+1)
!         if(work.lt.wrkmin)then
!           scale=fact
!           wrkmin=work
!           kopt=kk+1
!         endif
! 18    continue
!       hnext=h/scale
!       if(kopt.ge.k.and.kopt.ne.kmax.and..not.reduct)then
!         fact=max(scale/alf(kopt-1,kopt),SCALMX)
!         if(a(kopt+1)*fact.le.wrkmin)then
!           hnext=h/fact
!           kopt=kopt+1
!         endif
!       endif
!       return
!       END
! c
!       SUBROUTINE hypser(a,b,c,z,series,deriv)
!       implicit none
!       INTEGER n
!       COMPLEX*16 a,b,c,z,series,deriv,aa,bb,cc,fac,temp
!       deriv=cmplx(0.d0,0.d0,kind=kind(0d0))
!       fac=cmplx(1.d0,0.d0,kind=kind(0d0))
!       temp=fac
!       aa=a
!       bb=b
!       cc=c
!       do 11 n=1,1000
!         fac=fac*aa*bb/cc
!         deriv=deriv+fac
!         fac=fac*z/n
!         series=temp+fac
!         if (series.eq.temp) return
!         temp=series
!         aa=aa+1.d0
!         bb=bb+1.d0
!         cc=cc+1.d0
! 11    continue
!       pause 'convergence failure in hypser'
!       END
! c
!       SUBROUTINE hypdrv(s,y,dyds)
!       implicit none
!       REAL*8 s
!       COMPLEX*16 y(2),dyds(2),aa,bb,cc,z0,dz,z
!       COMMON /hypg/ aa,bb,cc,z0,dz
!       z=z0+s*dz
!       dyds(1)=y(2)*dz
!       dyds(2)=(aa*bb*y(1)-(cc-(aa+bb+1.d0)*z)*y(2))*dz/(z*(1.d0-z))
!       return
!       END
! c
!       SUBROUTINE mmid(y,dydx,nvar,xs,htot,nstep,yout)
!       implicit none
!       INTEGER nstep,nvar,NMAX
!       REAL*8 htot,xs,dydx(nvar),y(nvar),yout(nvar)
!       PARAMETER (NMAX=50)
!       INTEGER i,n
!       REAL*8 h,h2,swap,x,ym(NMAX),yn(NMAX)
!       h=htot/nstep
!       do 11 i=1,nvar
!         ym(i)=y(i)
!         yn(i)=y(i)+h*dydx(i)
! 11    continue
!       x=xs+h
!       call hypdrv(x,yn,yout)
!       h2=2.d0*h
!       do 13 n=2,nstep
!         do 12 i=1,nvar
!           swap=ym(i)+h2*yout(i)
!           ym(i)=yn(i)
!           yn(i)=swap
! 12      continue
!         x=x+h
!         call hypdrv(x,yn,yout)
! 13    continue
!       do 14 i=1,nvar
!         yout(i)=0.5d0*(ym(i)+yn(i)+h*yout(i))
! 14    continue
!       return
!       END
! c
!       SUBROUTINE pzext0(iest,xest,yest,yz,dy,nv)
!       implicit none
!       INTEGER iest,nv,IMAX,NMAX
!       REAL*8 xest,dy(nv),yest(nv),yz(nv)
!       PARAMETER (IMAX=13,NMAX=50)
!       INTEGER j,k1
!       REAL*8 delta,f1,f2,q,d(NMAX),qcol(NMAX,IMAX),x(IMAX)
!       SAVE qcol,x
!       x(iest)=xest
!       do 11 j=1,nv
!         dy(j)=yest(j)
!         yz(j)=yest(j)
! 11    continue
!       if(iest.eq.1) then
!         do 12 j=1,nv
!           qcol(j,1)=yest(j)
! 12      continue
!       else
!         do 13 j=1,nv
!           d(j)=yest(j)
! 13      continue
!         do 15 k1=1,iest-1
!           delta=1.d0/(x(iest-k1)-xest)
!           f1=xest*delta
!           f2=x(iest-k1)*delta
!           do 14 j=1,nv
!             q=qcol(j,k1)
!             qcol(j,k1)=dy(j)
!             delta=d(j)-q
!             dy(j)=f1*delta
!             d(j)=f2*delta
!             yz(j)=yz(j)+dy(j)
! 14        continue
! 15      continue
!         do 16 j=1,nv
!           qcol(j,iest)=dy(j)
! 16      continue
!       endif
!       return
!       END
! c
!!! /FB
