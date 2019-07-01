!!! module: shower_basics_module
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
!!! Latest Change: Thu Jan 13 17:21:18 2011 Time zone: 3600 seconds
!!! 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module shower_basics_module

  use kinds, only: default !NODEP!
  use constants, only : pi, twopi !NODEP!
  use tao_random_numbers !NODEP!

  implicit none

  public :: shower_pdf

  interface 
     subroutine shower_pdf (set, x, q, ff)
       integer, intent(in) :: set
       double precision, intent(in) :: x, q
       double precision, dimension(-6:6), intent(out) :: ff
     end subroutine shower_pdf
  end interface

  ! technical constants
  logical, parameter :: D_print=.false.     ! decides whether to print out additional information

  ! physical parameters
  real(default), public :: D_Min_t=1._default                ! cut-off scale t_cut, given in GeV^2  !! PARJ(82)
  real(default) :: D_min_scale=0.5_default           ! cut-off scale for Pt^2 ordered shower, given In GeV^2
  real(default) :: D_Lambda_fsr=0.29_default         !! PARP(72)
  real(default) :: D_Lambda_isr=0.29_default         !! PARP(61)

  ! settings
  integer :: D_Nf=5                 ! maximum number of flavours in gluon decay to quarks  !! MSTJ(45)
  ! decides whether to use constant or running alpha_s -> see function D_alpha_s(t) !! MSTJ(44) + MSTP(64)
  logical ::  D_running_alpha_s_fsr=.true.
  ! decides whether to use constant or running alpha_s -> see function D_alpha_s(t) !! MSTJ(44) + MSTP(64)
  logical ::  D_running_alpha_s_isr=.true.
  real(default) :: D_constalpha_s = 0.20_default  !! PARU(111)
  logical :: isr_pt_ordered = .false.
  !! set emitted timelike partons in spacelike shower on shell, true corresponds to MSTP(63)=0
  logical :: isr_only_onshell_emitted_partons = .true.
  logical :: isr_angular_ordered = .true.       ! whether isr is angular ordered, MSTP(62)
  logical :: treat_light_quarks_massless = .true.   ! treat d and u quarks as massless
  logical :: treat_duscb_quarks_massless = .false.     ! treat all quarks except t as massless

  ! varying parameters
  real(default) :: primordial_kt_width=1.5_default   ! width of Gaussian primordial kt distribution   !! PARP(91)
  real(default) :: primordial_kt_cutoff=5._default   ! cutoff for Gaussian primordial kt distribution !! PARP(93)
  real(default) :: maxz_isr=0.999_default  ! should be a parameter        !! PARP(66)
  real(default) :: minenergy_timelike=1._default    ! min energy of emitted timelike parton in isr  !! PARP(65)
  real(default) :: tscalefactor_isr=1._default    ! factor for first scale, default=1  ! should be a parameter
  ! factor, by which the integral in the sudhakov-factor is suppressed for the respective first scale in isr
  ! higher values -> higher starting scales
  real(default) :: first_integral_suppression_factor=1._default 

  ! auxiliary and temporaily paramters
  real(default) :: scalefactor1 = 0.02_default      ! temporary for Pt-ordered shower
  real(default) :: scalefactor2 = 0.02_default      ! temporary for Pt-ordered shower

  ! variable pdf functions
  procedure(shower_pdf), pointer :: shower_pdf_func
  integer :: shower_pdf_set = 0

contains

  subroutine randomseed(seed)
    integer, intent(in), optional :: seed
    integer :: clock

    if(present(seed)) then
       clock = seed
    else
       CALL SYSTEM_CLOCK(COUNT=clock)
    end if
    call tao_random_seed(clock)
  end subroutine randomseed

  function D_alpha_s_isr(tin) result(alpha_s)
    real(default), intent(in) :: tin
    real(default) :: b,t
    real(default) :: alpha_s

!    arbitrary lower cut off for scale
!    t=MAX(max(1._default*D_Min_t, 1.1_default*D_Lambda_isr**2), ABS(tin))
    t=max(max(0.1_default*D_Min_t, 1.1_default*D_Lambda_isr**2), abs(tin))

    if(D_running_alpha_s_isr) then
       b=(33._default-2._default*number_of_flavors(t))/(12._default*pi)
       alpha_s=1._default/(b*log(t/(D_Lambda_isr**2)))
    else
       alpha_s = D_constalpha_s
    end if
  end function D_alpha_s_isr

  function D_alpha_s_fsr(tin) result(alpha_s)
    real(default), intent(in) :: tin
    real(default) :: b,t
    real(default) :: alpha_s

!    arbitrary lower cut off for scale
!    t=MAX(max(1._default*D_Min_t, 1.1_default*D_Lambda**2), ABS(tin))
    t=max(max(0.1_default*D_Min_t, 1.1_default*D_Lambda_fsr**2), abs(tin))

    if(D_running_alpha_s_fsr) then
       b=(33._default-2._default*number_of_flavors(t))/(12._default*pi)
       alpha_s=1._default/(b*log(t/(D_Lambda_fsr**2)))
    else
       alpha_s = D_constalpha_s
    end if
  end function D_alpha_s_fsr

  function mass_typ(typ) result(mass)   ! mass in GeV
    integer, intent(in) :: typ
    real(default) :: mass

!!$    SELECT CASE(ABS(typ))
!!$        ! It is assumed that quark masses are ordered mass(1)<mass(2)<mass(3)<...
!!$    CASE (1) !d 
!!$       mass_typ=0.330_default
!!$    CASE (2) !u
!!$       mass_typ=0.330_default
!!$    CASE (3) !s
!!$       mass_typ=0.500_default
!!$    CASE (4) !c
!!$       mass_typ=1.500_default
!!$    CASE (5) !b
!!$       mass_typ=4.800_default
!!$    CASE (6) !t
!!$       mass_typ=175.00_default
!!$    CASE (2212) !proton
!!$       mass_typ=0.93827_default
!!$    CASE default !others not implemented
!!$       mass_typ=0.0_default
!!$    END SELECT

    mass = sqrt(mass_squared_typ(typ))   ! mass_typ probably not needed
  end function mass_typ

  function mass_squared_typ(typ) result(mass2)
    integer, intent(in) :: typ
    real(default) :: mass2

    select case(abs(typ))
        ! It is assumed that quark masses are ordered mass(1)<mass(2)<mass(3)<...
    case (1) !d 
       if(treat_light_quarks_massless.or.treat_duscb_quarks_massless) then
          mass2=0._default
       else
          mass2=0.330_default**2
       end if
    case (2) !u
       if(treat_light_quarks_massless.or.treat_duscb_quarks_massless) then
          mass2=0._default
       else
          mass2=0.330_default**2
       end if
    case (3) !s
       if(treat_duscb_quarks_massless) then
          mass2=0._default
       else
          mass2=0.500_default**2
       end if
    case (4) !c
       if(treat_duscb_quarks_massless) then
          mass2=0._default
       else
          mass2=1.500_default**2
       end if
    case (5) !b
       if(treat_duscb_quarks_massless) then
          mass2=0._default
       else
          mass2=4.800_default**2
       end if
    case (6) !t
       mass2=175.00_default**2
    case (21) ! Gluon
       mass2=0.0_default
    case (2112) !neutron
       mass2=0.939565_default**2
    case (2212) !proton
       mass2=0.93827_default**2
       ! other mesons and baryons needed for beam-remnant
    case (411) ! D+
       mass2=1.86960_default**2
    case (421) ! D0
       mass2=1.86483_default**2
    case (511) ! B0
       mass2=5.27950_default**2
    case (521) ! B+
       mass2=5.27917_default**2
    case (2224) !Delta++
       mass2=1.232_default**2
    case (3212) !Sigma0
       mass2=1.192642_default**2
    case (3222) !Sigma+
       mass2=1.18937_default**2
    case (4212) ! Sigma_c+
       mass2=2.4529_default**2
    case (4222) ! Sigma_c++
       mass2=2.45402_default**2
    case (5212) ! Sigma_b0
       mass2=5.8152_default**2
    case (5222) ! Sigma_b+
       mass2=5.8078_default**2
    case (0) ! I take 0 to be partons whose type is not yet clear
       mass2=0.0_default
    case (9999) ! beam remnant
       mass2 = 0.0_default ! don't know how to handle the beamremnant
    case default !others not implemented
       mass2=0.0_default
!       print *, " error in mass_squared_typ: typ not known"
    end select
  end function mass_squared_typ

  function number_of_flavors(t) result(nr)          ! number of flavours allowed in an actual g->qq decay
    real(default), intent(in) :: t
    integer :: nr

    integer :: i

    nr=0
    if(t < 0.25_default*D_Min_t) return   ! arbitrary cut off ?WRONG?
    do i=1,min(D_Nf,3)    ! to do: take heavier quarks(-> cuts on allowed costheta in g->qq) into account
       if( (4._default*mass_squared_typ(i)+D_Min_t) < t ) then
          nr=i
       else
          exit
       end if
    end do
  end function number_of_flavors

  function P_qqg(z) result(P)               ! quark => quark + gluon
    real(default), intent(in) :: z
    real(default) :: P
    
    P=(4._default/3._default)*(1._default+z**2)/(1._default-z)
  end function P_qqg

  function P_gqq(z) result(P)               ! gluon => quark + antiquark
    real(default), intent(in) :: z
    real(default) :: P
    
    P=0.5_default*(z**2+(1._default-z)**2)
    ! anti-symmetrized version -> needs change of first and second daughter in 50% of branchings
    !    P=(1._default-z)**2
  end function P_gqq

  function P_ggg(z) result(P)               ! gluon => gluon + gluon
    real(default), intent(in) :: z
    real(default) :: P
    
    P=3._default*( (1._default-z)/z + z/(1._default-z) + z*(1._default-z) )
    ! anti-symmetrized version -> needs to by symmetrized in color connections
    !    P=3._default*( 2._default*z/(1._default-z) + z*(1._default-z) )
  end function P_ggg

  function integral_over_P_gqq(zmin, zmax) result (integral)
    real(default), intent(in) :: zmin, zmax
    real(default) :: integral

    integral=0.5_default*( (2._default/3._default)*(zmax**3-zmin**3) - (zmax**2 - zmin**2) + (zmax - zmin) )
  end function integral_over_P_gqq

  function integral_over_P_ggg(zmin, zmax) result (integral)
    real(default), intent(in) :: zmin, zmax
    real(default) :: integral

    integral=3._default*((log(zmax)-zmax-zmax-log(1._default-zmax)+zmax**2/2._default-zmax**3/3._default)-&
         (log(zmin)-zmin-zmin-log(1._default-zmin)+zmin**2/2._default-zmin**3/3._default) )
  end function integral_over_P_ggg

  function integral_over_P_qqg(zmin, zmax) result (integral)
    real(default), intent(in) :: zmin, zmax
    real(default) :: integral

    integral=(2._default/3._default)*(-zmax**2+zmin**2-2._default*(zmax-zmin)+4._default*log((1._default-zmin)/(1._default-zmax)) )
  end function integral_over_P_qqg

  !! methods to set parameters -> better interface?

  subroutine shower_set_D_Min_t(input)
    real(default) :: input
    D_Min_t = input
  end subroutine shower_set_D_Min_t

  subroutine shower_set_D_Lambda_fsr(input)
    real(default) :: input
    D_Lambda_fsr = input
  end subroutine shower_set_D_Lambda_fsr

  subroutine shower_set_D_Lambda_isr(input)
    real(default) :: input
    D_Lambda_isr = input
  end subroutine shower_set_D_Lambda_isr

  subroutine shower_set_D_Nf(input)
    integer :: input
    D_Nf = input
  end subroutine shower_set_D_Nf

  subroutine shower_set_D_running_alpha_s_fsr(input)
    logical :: input
    D_running_alpha_s_fsr = input
  end subroutine shower_set_D_running_alpha_s_fsr

  subroutine shower_set_D_running_alpha_s_isr(input)
    logical :: input
    D_running_alpha_s_isr = input
  end subroutine shower_set_D_running_alpha_s_isr

  subroutine shower_set_D_constantalpha_s(input)
    real(default) :: input
    D_constalpha_s = input
  end subroutine shower_set_D_constantalpha_s

  subroutine shower_set_isr_pt_ordered(input)
    logical :: input
    isr_pt_ordered = input
  end subroutine shower_set_isr_pt_ordered

  subroutine shower_set_isr_angular_ordered(input)
    logical :: input
    isr_angular_ordered = input
  end subroutine shower_set_isr_angular_ordered

  subroutine shower_set_primordial_kt_width(input)
    real(default) :: input
    primordial_kt_width = input
  end subroutine shower_set_primordial_kt_width

  subroutine shower_set_primordial_kt_cutoff(input)
    real(default) :: input
    primordial_kt_cutoff = input
  end subroutine shower_set_primordial_kt_cutoff

  subroutine shower_set_maxz_isr(input)
    real(default) :: input
    maxz_isr = input
  end subroutine shower_set_maxz_isr

  subroutine shower_set_minenergy_timelike(input)
    real(default) :: input
    minenergy_timelike = input
  end subroutine shower_set_minenergy_timelike

  subroutine shower_set_tscalefactor_isr(input)
    real(default) :: input
    tscalefactor_isr = input
  end subroutine shower_set_tscalefactor_isr

  subroutine shower_set_isr_only_onshell_emitted_partons(input)
    logical :: input
    isr_only_onshell_emitted_partons = input
  end subroutine shower_set_isr_only_onshell_emitted_partons

  subroutine shower_set_pdf_func(func)
    procedure(shower_pdf), pointer, intent(in) :: func
    shower_pdf_func => func
  end subroutine shower_set_pdf_func
  
  subroutine shower_set_pdf_set(set)
    integer, intent(in) :: set
    shower_pdf_set = set
  end subroutine shower_set_pdf_set
end module shower_basics_module
