!!! module: shower_basics_module
!!! This code is part of my Ph.D studies.
!!! 
!!! Copyright (C) 2010 Sebastian Schmidt <sebastian.schmidt@physik.uni-freiburg.de>
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
!!! Latest Change: Thu Jul  1 16:12:04 2010 Time zone: 7200 seconds
!!! 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module shower_basics_module

  use kinds, only: double !NODEP!
  use constants, only : pi, twopi !NODEP!
  use tao_random_numbers !NODEP!

  implicit none

  ! technical constants
  logical, parameter :: D_print=.false.	   ! decides whether to print out additional information

  ! physical parameters
  real(kind=double) :: D_Min_t=1._double 	! cut-off scale t_cut, given in GeV^2  !! PARJ(82)
  real(Kind=Double) :: D_min_scale=0.5_double 	! Cut-Off Scale For Pt^2 Ordered Shower, Given In Gev^2
  real(kind=double) :: D_Lambda_fsr=0.29_double         !! PARP(72)
  real(kind=double) :: D_Lambda_isr=0.29_double         !! PARP(61)

  ! settings
  integer :: D_Nf=5	           ! maximum number of flavours in gluon decay to quarks  !! MSTJ(45)
  ! decides whether to use constant or running alpha_s -> see function D_alpha_s(t) !! MSTJ(44) + MSTP(64)
  logical ::  D_running_alpha_s_fsr=.true.
  ! decides whether to use constant or running alpha_s -> see function D_alpha_s(t) !! MSTJ(44) + MSTP(64)
  logical ::  D_running_alpha_s_isr=.true.
  real(kind=double) :: D_constalpha_s = 0.20_double  !! PARU(111)
  logical :: isr_pt_ordered = .false.
  !! set emitted timelike partons in spacelike shower on shell, true corresponds to MSTP(63)=0
  logical :: isr_only_onshell_emitted_partons = .true.
  logical :: isr_angular_ordered = .true.       ! whether isr is angular ordered, MSTP(62)

  ! varying parameters
  real(kind=double) :: primordial_kt_width=1.5_double   ! width of Gaussian primordial kt distribution   !! PARP(91)
  real(kind=double) :: primordial_kt_cutoff=5._double   ! cutoff for Gaussian primordial kt distribution !! PARP(93)
  real(kind=double) :: maxz_isr=0.999_double  ! should be a parameter        !! PARP(66)
  real(kind=double) :: minenergy_timelike=1._double    ! min energy of emitted timelike parton in isr  !! PARP(65)
  real(kind=double) :: tscalefactor_isr=0.075_double    ! factor for first scale, default=1  ! should be a parameter
  ! factor, by which the integral in the sudhakov-factor is suppressed for the respective first scale in isr
  ! higher values -> more activity
  real(kind=double) :: first_integral_suppression_factor=2._double 

  ! auxiliary and temporaily paramters
  real(kind=double) :: scalefactor1 = 0.02_double      ! temporary for Pt-ordered shower
  real(kind=double) :: scalefactor2 = 0.02_double      ! temporary for Pt-ordered shower

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
    real(kind=double), intent(in) :: tin
    real(kind=double) :: b,t
    real(kind=double) :: alpha_s

!    arbitrary lower cut off for scale
!    t=MAX(max(1._double*D_Min_t, 1.1_double*D_Lambda**2), ABS(tin))
    t=max(max(0.1_double*D_Min_t, 1.1_double*D_Lambda_isr**2), abs(tin))

    if(D_running_alpha_s_isr) then
       b=(33._double-2._double*number_of_flavors(t))/(12._double*pi)
       alpha_s=1._double/(b*log(t/(D_Lambda_isr**2)))
    else
       alpha_s = D_constalpha_s
    end if
  end function D_alpha_s_isr

  function D_alpha_s_fsr(tin) result(alpha_s)
    real(kind=double), intent(in) :: tin
    real(kind=double) :: b,t
    real(kind=double) :: alpha_s

!    arbitrary lower cut off for scale
!    t=MAX(max(1._double*D_Min_t, 1.1_double*D_Lambda**2), ABS(tin))
    t=max(max(0.1_double*D_Min_t, 1.1_double*D_Lambda_fsr**2), abs(tin))

    if(D_running_alpha_s_fsr) then
       b=(33._double-2._double*number_of_flavors(t))/(12._double*pi)
       alpha_s=1._double/(b*log(t/(D_Lambda_fsr**2)))
    else
       alpha_s = D_constalpha_s
    end if
  end function D_alpha_s_fsr

  function mass_typ(typ) result(mass)   ! mass in GeV
    integer, intent(in) :: typ
    real(kind=double) :: mass

!!$    SELECT CASE(ABS(typ))
!!$        ! It is assumed that quark masses are ordered mass(1)<mass(2)<mass(3)<...
!!$    CASE (1) !d 
!!$       mass_typ=0.330_double
!!$    CASE (2) !u
!!$       mass_typ=0.330_double
!!$    CASE (3) !s
!!$       mass_typ=0.500_double
!!$    CASE (4) !c
!!$       mass_typ=1.500_double
!!$    CASE (5) !b
!!$       mass_typ=4.800_double
!!$    CASE (6) !t
!!$       mass_typ=175.00_double
!!$    CASE (2212) !proton
!!$       mass_typ=0.93827_double
!!$    CASE default !others not implemented
!!$       mass_typ=0.0_double
!!$    END SELECT

    mass = sqrt(mass_squared_typ(typ))   ! mass_typ probably not needed
  end function mass_typ

  function mass_squared_typ(typ) result(mass2)
    integer, intent(in) :: typ
    real(kind=double) :: mass2

    select case(abs(typ))
        ! It is assumed that quark masses are ordered mass(1)<mass(2)<mass(3)<...
    case (1) !d 
       mass2=0.330_double**2
    case (2) !u
       mass2=0.330_double**2
    case (3) !s
       mass2=0.500_double**2
    case (4) !c
       mass2=1.500_double**2
    case (5) !b
       mass2=4.800_double**2
    case (6) !t
       mass2=175.00_double**2
    case (2212) !proton
       mass2=0.93827_double**2
    case (21) ! Gluon
       mass2=0.0_double
    case (0) ! I take 0 to be partons whose type is not yet clear
       mass2=0.0_double
    case (9999) ! beam remnant
       mass2 = 0.0_double ! don't know how to handle the beamremnant
    case default !others not implemented
       mass2=0.0_double
!       print *, " error in mass_squared_typ: typ not known"
    end select
  end function mass_squared_typ

  function number_of_flavors(t) result(nr)		! number of flavours allowed in an actual g->qq decay
    real(kind=double), intent(in) :: t
    integer :: nr

    integer :: i

    nr=0
    if(t < 0.25_double*D_Min_t) return   ! arbitrary cut off ?WRONG?
    do i=1,min(D_Nf,3)    ! to do: take heavier quarks(-> cuts on allowed costheta in g->qq) into account
       if( (4._double*mass_squared_typ(i)+D_Min_t) < t ) then
          nr=i
       else
          exit
       end if
    end do
  end function number_of_flavors

  function P_qqg(z) result(P)               ! quark => quark + gluon
    real(kind=double), intent(in) :: z
    real(kind=double) :: P
    
    P=(4._double/3._double)*(1._double+z**2)/(1._double-z)
  end function P_qqg

  function P_gqq(z) result(P)               ! gluon => quark + antiquark
    real(kind=double), intent(in) :: z
    real(kind=double) :: P
    
    P=0.5_double*(z**2+(1._double-z)**2)
    ! anti-symmetrized version -> needs change of first and second daughter in 50% of branchings
    !    P=(1._double-z)**2
  end function P_gqq

  function P_ggg(z) result(P)               ! gluon => gluon + gluon
    real(kind=double), intent(in) :: z
    real(kind=double) :: P
    
    P=3._double*( (1._double-z)/z + z/(1._double-z) + z*(1._double-z) )
    ! anti-symmetrized version -> needs to by symmetrized in color connections
    !    P=3._double*( 2._double*z/(1._double-z) + z*(1._double-z) )
  end function P_ggg

  !! methods to set parameters -> better interface?

  subroutine shower_set_D_Min_t(input)
    real(kind=double) :: input
    D_Min_t = input
  end subroutine shower_set_D_Min_t

  subroutine shower_set_D_Lambda_fsr(input)
    real(kind=double) :: input
    D_Lambda_fsr = input
  end subroutine shower_set_D_Lambda_fsr

  subroutine shower_set_D_Lambda_isr(input)
    real(kind=double) :: input
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
    real(kind=double) :: input
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
    real(kind=double) :: input
    primordial_kt_width = input
  end subroutine shower_set_primordial_kt_width

  subroutine shower_set_primordial_kt_cutoff(input)
    real(kind=double) :: input
    primordial_kt_cutoff = input
  end subroutine shower_set_primordial_kt_cutoff

  subroutine shower_set_maxz_isr(input)
    real(kind=double) :: input
    maxz_isr = input
  end subroutine shower_set_maxz_isr

  subroutine shower_set_minenergy_timelike(input)
    real(kind=double) :: input
    minenergy_timelike = input
  end subroutine shower_set_minenergy_timelike

  subroutine shower_set_isr_only_onshell_emitted_partons(input)
    logical :: input
    isr_only_onshell_emitted_partons = input
  end subroutine shower_set_isr_only_onshell_emitted_partons
  
end module shower_basics_module
