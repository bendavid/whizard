! WHIZARD <<Version>> <<Date>>

! Copyright (C) 1999-2015 by 
!     Wolfgang Kilian <kilian@physik.uni-siegen.de>
!     Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
!     Juergen Reuter <juergen.reuter@desy.de>
!     with contributions from
!     Fabian Bach <fabian.bach@desy.de>
!     Bijan Chokoufe <bijan.chokoufe@desy.de>
!     Christian Speckner <cnspeckn@googlemail.com>
!     Marco Sekulla <marco.sekulla@desy.de>
!     Christian Weiss <christian.weiss@desy.de>
!     Felix Braam, Sebastian Schmidt
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
  use constants
  use physics_defs, only: CF, CA, TR
  use sm_physics
  use interpolation !NODEP!
  use nr_tools
  use io_units, only: free_unit
  use iso_varying_string, string_t => varying_string !NODEP!
  use system_dependencies !NODEP!
  use iso_fortran_env !NODEP!
  use diagnostics !NODEP!
  use solver, only: solver_function_t, solve_qgaus
  implicit none
  save
  private

  logical :: ext_Vinput = .true.
  logical :: ext_NLO = .false.

  logical :: init_pars, init_ps, init_ff, init_J0
  real(default) :: m1s, gam
  integer :: nloop
  real(default) :: mtpole = -1.0_default
  real(default) :: mtpole_init
  real(default) :: h, mu_h, ah
  real(default) :: f, mu_s, as, asLL, nustar_fixed
  real(default), parameter :: nustar_offset = 0.05_default
  logical :: nustar_dynamic, need_ff, need_J0, need_p0, match_to_NLO
  real(default) :: mu_u, au
  real(default), parameter :: nf = 5.0_default
  real(default) :: a1, a2, b0, b1
  real(default), dimension(2) :: aa2, aa3, aa4, aa5, aa8, aa0
  real(default) :: sqrts_ref = -1.0_default
  complex(default) :: ieps = imago*tiny_10, ximo = 0.0_default
  integer :: ff_type, matching_version
  character(len=200) :: parameters_ref = ""
  type(nr_spline_t) :: ff_p_spline
  real(default) :: v1, v2

  integer :: n_sq, n_p, n_p0, n_p_p0dep, n_q
  real(default), dimension(:), allocatable :: sq_grid, p_grid, p0_grid, q_grid, p_grid_fine
  complex(default), dimension(:,:,:,:), allocatable :: ff_grid
  complex(default), dimension(:,:,:), allocatable :: J0_grid
  complex(single), dimension(:,:,:,:,:), allocatable :: Vmatrix

  !!! explicit range and step size of the sqrts-grid relative to 2*m1S:
  !!! step size should be reduced to 0.1 before release
!  real(default), parameter :: sqrts_lo=-4.0_default, sqrts_hi=96.0_default, sqrts_it=0.5_default
  !!! fast setup for sqrts = 2*m1S, sufficient to run SM_tt_threshold_test.sin
  real(default), parameter :: sqrts_lo=-10.0_default, sqrts_hi=20.0_default, sqrts_it=0.2_default


  interface char
    module procedure int_to_char, real_to_char, complex_to_char, logical_to_char
  end interface char

  interface formfactor_LL_analytic
    module procedure formfactor_LL_analytic, formfactor_LL_analytic_p0
  end interface formfactor_LL_analytic

  interface ttv_formfactors_init_parameters
    module procedure init_parameters
  end interface ttv_formfactors_init_parameters
  public :: ttv_formfactors_init_parameters

  interface ttv_formfactors_init_threshold_grids
    module procedure init_threshold_grids
  end interface ttv_formfactors_init_threshold_grids
  public :: ttv_formfactors_init_threshold_grids

  interface ttv_formfactors_FF
    module procedure FF_master
  end interface ttv_formfactors_FF
  public :: ttv_formfactors_FF

  interface ttv_formfactors_m1s_to_mpole
    module procedure m1s_to_mpole
  end interface ttv_formfactors_m1s_to_mpole
  public :: ttv_formfactors_m1s_to_mpole

  type, public :: phase_space_point_t
    real(default) :: p2 = 0, k2 = 0, q2 = 0
    real(default) :: sqrts = 0, p = 0, p0 = 0
    real(default) :: mpole = 0, en = 0
    complex(default) :: m2 = 0
    logical :: inside_grid = .false., onshell = .false.
  contains
    procedure :: init => phase_space_point_init_rel
    procedure :: init_nonrel => phase_space_point_init_nonrel
    procedure :: is_onshell => phase_space_point_is_onshell
  end type phase_space_point_t

  type, extends (solver_function_t) :: p0_q_integrand_t
    real(default) :: a = 0
    type(phase_space_point_t) :: ps
    integer :: i = 0
  contains
    procedure :: update => p0_q_integrand_update
    procedure :: evaluate => p0_q_integrand_evaluate
  end type p0_q_integrand_t

contains

  subroutine init_parameters (mpole_out, gam_out, m1s_in, Vtb, gam_inv, aemi, sw, &
                              az, mz, mw, mb, h_in, f_in, nloop_in, ff_in, v1_in, v2_in)
    real(default), intent(out) :: mpole_out
    real(default), intent(out) :: gam_out
    real(default), intent(in) :: m1s_in
    real(default), intent(in) :: Vtb
    real(default), intent(in) :: gam_inv
    real(default), intent(in) :: aemi
    real(default), intent(in) :: sw
    real(default), intent(in) :: az
    real(default), intent(in) :: mz
    real(default), intent(in) :: mw
    real(default), intent(in) :: mb
    real(default), intent(in) :: h_in
    real(default), intent(in) :: f_in
    real(default), intent(in) :: nloop_in
    real(default), intent(in) :: ff_in
    real(default), intent(in) :: v1_in
    real(default), intent(in) :: v2_in
    real(default) :: z3
    !!! possibly (re-)enable these as user parameters:
    real(default) :: nu_in = -1
    init_pars = .false.
    m1s = m1s_in
    !!! compute the total LO top width from t->bW decay plus optional invisible width
    gam = total_top_width_LO (one / aemi, sw, Vtb, m1s, mw, mb) + gam_inv
    gam_out = gam
    nloop = 1
    if ( int(nloop_in) > nloop ) then
      call msg_warning ("reset to highest available nloop = " // char(nloop))
    else
      nloop = int(nloop_in)
    end if
    v1 = v1_in
    v2 = v2_in

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
         (20./9.*TR*nf)**2)
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

    !!! soft parameters incl. mtpole (depend on sqrts: initialize with sqrts ~ 2*m1s)
    nustar_fixed = nu_in
    nustar_dynamic = ( nustar_fixed  < 0. )
    f = f_in
    call update_soft_parameters ( 2.*m1s )
    mtpole_init = mtpole
    mpole_out = mtpole_init

    !!! flags
    ff_type = max (int(ff_in), 0)
    matching_version = 0
    if ( ff_in < 0.0_default ) matching_version = int(-ff_in)
    match_to_NLO = ( matching_version == 1 ) .or. ( matching_version == 2 )
    need_ff = ff_type <= 1
    need_J0 = ff_type == 2 .or. (match_to_NLO .and. .not. ext_NLO) .or. ff_type == 5
    need_p0 = ff_type == 0 .or. need_J0
    ext_Vinput = ext_Vinput .and. ( nloop > 0 )

    init_pars = .true.
  end subroutine init_parameters
  
  subroutine init_threshold_grids (test)
    real(default), intent(in) :: test
    if ( test > 0.0_default ) then
      call msg_message ("TESTING ONLY: Skip threshold initialization and use tree-level SM.")
      return
    end if
    if ( .not.init_pars ) call msg_fatal ("init_threshold_grid: parameters not initialized!")
    if ( parameters_ref == parameters_string () ) return
    call dealloc_grids ()
    if ( need_ff ) call init_formfactor_grid ()
    if ( need_J0 ) call scan_J0_over_phase_space_grid ()
    parameters_ref = parameters_string ()
  end subroutine init_threshold_grids

  pure function FF_master (ps, i) result (c)
    type(phase_space_point_t), intent(in) :: ps
    integer, intent(in) :: i
    complex(default) :: c
    c = one
    if (.not. init_pars) return
    !!! on-shell veto
    if (ps%onshell) return
    select case (ff_type)
      case (0)
        c = matched_formfactor (ps, i)
      case (1)
        c = resummed_formfactor (ps, i)
      case (2)
        c = relativistic_formfactor_pure (ah, ps, i)
      case (3)
        c = nonrelativistic_formfactor (ah, ps, i)
      case (5)
        c = nonrelativistic_formfactor (alphas_soft(ps%sqrts,nloop)-ah, ps, i) &
            + relativistic_formfactor_pure (ah, ps, i) - one
      case default
        return
    end select
  end function FF_master

  !!! matched formfactor (-> resummation in threshold region, smooth continuation above)
  pure function matched_formfactor (ps, i) result (c)
    type(phase_space_point_t), intent(in) :: ps
    integer, intent(in) :: i
    complex(default) :: c
    c = one
    if ( matching_version > 0 ) then
      if ( ps%inside_grid ) then
        c = resummed_formfactor (ps, i)
      else if ( match_to_NLO ) then
        c = nonrelativistic_formfactor ((alphas_soft(ps%sqrts,nloop)-ah), ps, i)
      end if
      if ( match_to_NLO .and. .not.ext_NLO ) c = c + &
        relativistic_formfactor_pure (ah, ps, i) - one
    else
      c = resummed_formfactor (ps, i)
    end if
  end function matched_formfactor

  !!! LL/NLL resummation of nonrelativistic Coulomb potential
  pure function resummed_formfactor (ps, i) result (c)
    type(phase_space_point_t), intent(in) :: ps
    integer, intent(in) :: i
    complex(default) :: c
    c = one
    if ( .not.init_ff .or. .not.ps%inside_grid ) return
    if ( need_p0 ) then
      if ( i == 2 ) return
      call interpolate_linear (sq_grid, p_grid, p0_grid, ff_grid(:,:,:,i), &
        ps%sqrts, ps%p, ps%p0, c)
    else
      call interpolate_linear (sq_grid, p_grid, ff_grid(:,:,1,i), ps%sqrts, ps%p, c)
    end if
  end function resummed_formfactor

  !!! relativistic off-shell O(alphas^1) contribution (-> no resummation)
  function relativistic_formfactor (alphas, ps, i) result (c)
    real(default), intent(in) :: alphas
    type(phase_space_point_t), intent(in) :: ps
    integer, intent(in) :: i
    complex(default) :: c
    complex(default) :: J0
    complex(default) :: J0_LoopTools
    external J0_LoopTools
    c = one
    if ( .not.init_pars .or. i==2 ) return
    J0 = J0_LoopTools (ps%p2, ps%k2, ps%q2, ps%m2)
    c = formfactor_ttv_relativistic_nlo (alphas, ps, J0)
  end function relativistic_formfactor

  !!! relativistic off-shell O(alphas^1) contribution (-> no resummation)
  !!! pure version requiring an existing J0 grid to avoid non-pure LoopTools calls
  pure function relativistic_formfactor_pure (alphas, ps, i) result (c)
    real(default), intent(in) :: alphas
    type(phase_space_point_t), intent(in) :: ps
    integer, intent(in) :: i
    complex(default) :: c
!    complex(default) :: J0
    c = one
    if ( i==2 ) return
!    J0 = J0_LoopTools_interpolate (ps)
!    c = formfactor_ttv_relativistic_nlo (alphas, ps, J0)
    c = one + alphas * J0_LoopTools_interpolate (ps)
  end function relativistic_formfactor_pure

  !!! leading nonrelativistic O(alphas^1) contribution (-> no resummation)
  !!! nonrelativistic limit of module function 'relativistic_formfactor'
  pure function nonrelativistic_formfactor (alphas, ps, i) result (c)
    real(default), intent(in) :: alphas
    type(phase_space_point_t), intent(in) :: ps
    integer, intent(in) :: i
    complex(default) :: c
    real(default) :: m, p
    complex(default) :: v
    c = one
    if ( .not.init_pars .or. i==2 ) return
    m = ps%mpole
    p = ps%p
    v = sqrts_to_v (ps%sqrts)
    c = c + alphas * ( &
        !!! O(alphas) contribution from potential
        0.5_default*imago*CF*m*log( (p+m*v+ps%p0)/(-p+m*v+ps%p0) +ieps) / p &
        !!! shift from hard current coefficient
        -2.0_default*CF/pi )
  end function nonrelativistic_formfactor

  subroutine init_formfactor_grid ()
    type(string_t) :: ff_file
    call msg_debug (D_THRESHOLD, "init_formfactor_grid")
    init_ff = .false.
    ff_file = "SM_tt_threshold.grid"
    call msg_message ()
    call msg_message ("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
    call msg_message (" Initialize e+e- => ttbar threshold resummation:")
    call msg_message (" Use analytic (LL) or TOPPIK (NLL) form factors for ttA/ttZ vector")
    call msg_message (" and axial vector couplings (S/P-wave) in the threshold region.")
    call msg_message (" Cf. threshold shapes from A. Hoang et al.: [arXiv:hep-ph/0107144],")
    call msg_message (" [arXiv:1309.6323].")
    if ( nloop > 0 ) then
      call msg_message (" Numerical NLL solutions calculated with TOPPIK [arXiv:hep-ph/9904468]")
      call msg_message (" by M. Jezabek, T. Teubner.")
    end if
    call msg_message ("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
    call msg_message ()
    call read_formfactor_grid (ff_file)
    if (.not. init_ff) then
      if (.not. init_ps) call init_threshold_phase_space_grid ()
      call scan_formfactor_over_phase_space_grid ()
      call write_formfactor_grid (ff_file)
    end if
  end subroutine init_formfactor_grid

  subroutine read_formfactor_grid (ff_file)
    type(string_t), intent(in) :: ff_file
    complex(single), dimension(:,:,:,:), allocatable :: ff_grid_sp
    character(len(parameters_ref)) :: parameters
    integer :: u, st
    logical :: ex
    integer, dimension(4) :: ff_shape
    call msg_debug (D_THRESHOLD, "read_formfactor_grid")
    inquire (file=char(ff_file), exist=ex)
    if (.not. ex) return
    u = free_unit ()
    call msg_message ("Opening grid file: " // char(ff_file))
    open (unit=u, status='old', file=char(ff_file), form='unformatted', iostat=st)
    if (st /= 0) call msg_fatal ("iostat = " // char(st))
    read (u) parameters
    read (u) ff_shape
    if (ff_shape(4) /= 2)  call msg_fatal ("read_formfactor_grid: i = " // char(ff_shape(4)))
    if (parameters /= parameters_string ()) then
       call msg_message ("Threshold setup has changed: recalculate threshold grid.")
       close (unit=u, status='delete')
       return
    end if
    call msg_message ("Threshold setup unchanged: reusing existing threshold grid.")
    n_sq = ff_shape(1)
    allocate (sq_grid(n_sq))
    read (u) sq_grid
    allocate (p_grid(n_p))
    read (u) p_grid
    if (need_p0) then
       n_p0 = ff_shape(3)
       call init_p0_grid (p_grid, n_p0)
    end if
    allocate (ff_grid_sp(n_sq,n_p,n_p0,2))
    read (u) ff_grid_sp
    allocate (ff_grid(n_sq,n_p,n_p0,2))
    ff_grid = cmplx (ff_grid_sp, kind=default)
    deallocate (ff_grid_sp)
    close (u, iostat=st)
    if (st > 0)  call msg_fatal ("close " // char(ff_file) // ": iostat = " // char(st))
    init_ps = .true.
    init_ff = .true.
  end subroutine read_formfactor_grid

  subroutine write_formfactor_grid (ff_file)
    type(string_t), intent(in) :: ff_file
    integer :: u, st
    if (.not. init_ff) then
      call msg_warning ("write_formfactor_grid: no grids initialized!")
      return
    end if
    u = free_unit ()
    open (unit=u, status='replace', file=char(ff_file), form='unformatted', iostat=st)
    if (st /= 0)  call msg_fatal ("open " // char(ff_file) // ": iostat = " // char(st))
    write (u) parameters_string ()
    write (u) shape(ff_grid)
    write (u) sq_grid
    write (u) p_grid
    write (u) cmplx(ff_grid, kind=single)
    close (u, iostat=st)
    if (st > 0)  call msg_fatal ("close " // char(ff_file) // ": iostat = " // char(st))
  end subroutine write_formfactor_grid

  pure function parameters_string () result (str)
    character(len(parameters_ref)) :: str
    str = char(m1s) // " " // char(gam) // " " // char(nloop) // " " // char(h) &
           // " " // char(f) // " " // char(need_p0) // " " // char(ff_type) &
           // " " // char(matching_version) //  " " // char(sqrts_lo) &
           // " " // char(sqrts_hi) // " " // char(sqrts_it)
  end function parameters_string

  subroutine update_soft_parameters (sqrts)
    real(default), intent(in) :: sqrts
    real(default) :: nusoft
    if (.not. nustar_dynamic .and. mtpole > 0.0_default) return
    if (init_pars .and. is_equal (sqrts, sqrts_ref)) return
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
  end subroutine update_soft_parameters

  !!! Coulomb potential coefficients needed by TOPPIK
  pure function xc (a_soft, i_xc) result (xci)
    real(default), intent(in) :: a_soft
    integer, intent(in) :: i_xc
    real(default) :: xci
    xci = 0.0_default
    select case (i_xc)
      case (0)
        xci = one
        if ( nloop>0 ) xci = xci + a_soft/(4.*pi) * a1
        if ( nloop>1 ) xci = xci + (a_soft/(4.*pi))**2 * a2
      case (1)
        if ( nloop>0 ) xci = xci + a_soft/(4.*pi) * b0
        if ( nloop>1 ) xci = xci + (a_soft/(4.*pi))**2 * (b1 + 2*b0*a1)
      case (2)
        if ( nloop>1 ) xci = xci + (a_soft/(4.*pi))**2 * b0**2
      case default
        return
    end select
  end function xc

  function current_coeff (a_hard, a_soft, a_usoft, i) result (coeff)
    real(default), intent(in) :: a_hard
    real(default), intent(in) :: a_soft
    real(default), intent(in) :: a_usoft
    integer, intent(in) :: i
    real(default) :: coeff
    real(default) :: matching_c, c1
    real(default) :: z, w
    coeff = one
    if ( nloop == 0 ) return
    z = a_soft / a_hard
    w = a_usoft / a_soft
    !!! hard s/p-wave 1-loop matching coefficients, cf. arXiv:hep-ph/0604072
    select case (i)
      case (1)
        matching_c = one - 2.*(CF/pi) * a_hard
      case (2)
        matching_c = one -    (CF/pi) * a_hard
     case default
        call msg_fatal ("current_coeff: unknown coeff i = " // char(i))
    end select
    !!! current coefficient c1, cf. arXiv:hep-ph/0609151, Eq. (62)
    c1 = exp( a_hard * pi * ( aa2(i)*(1.-z) + aa3(i)*log(z) + &
         aa4(i)*(1.-z**(1.-13.*CA/(6.*b0))) + aa5(i)*(1.-z**(1.-2.*CA/b0)) + &
         aa8(i)*(1.-z**(1.-CA/b0)) + aa0(i)*(z-1.-log(w)/w) ))
    coeff = matching_c * c1
  end function current_coeff

  !!! matching parameter as a function of the phase space point
  pure function v_matching (ps) result (v)
    type(phase_space_point_t), intent(in) :: ps
    real(default) :: v
!    v = real( sqrts_to_v (ps%sqrts) )
    !!! Andre's proposal for the switch-off function:
    v = max ( sqrt(abs(ps%p2-ps%m2)), sqrt(abs(ps%k2-ps%m2)), &
              sqrt(abs(ps%q2/4.-ps%m2)), ps%p ) &
        / ps%mpole
  end function v_matching

  !!! measure for the validity of the nonrelativistic approximation
  pure function A_matching (alphas, ps) result (A)
    real(default), intent(in) :: alphas
    type(phase_space_point_t), intent(in) :: ps
    real(default) :: A
    complex(default) :: FF_relat, FF_nonrel
    FF_relat = relativistic_formfactor_pure (alphas, ps, 1)
    FF_nonrel = nonrelativistic_formfactor (alphas, ps, 1)
    A = abs( (FF_relat-FF_nonrel) / (FF_relat+FF_nonrel) )
  end function A_matching

  !!! smooth transition from f1 to f2 between v1 and v2 (2 combined parabolas)
  pure function f_matching (v) result (fval)
    real(default), intent(in) :: v
    real(default) :: fval
    real(default) :: vm, f1, f2
!     real(default) :: v1, v2
!     v1 = 0.3_default !!! LO v1
!     v2 = 0.5_default
!     v1 = 0.2_default !!! LO v2
!     v2 = 0.7_default
!     v1 = 0.25_default !!! LO v3
!     v2 = 0.75_default
!     v1 = 0.25_default !!! LO v4
!     v2 = 0.7_default
!     v1 = 0.5_default !!! LO v5
!     v2 = 0.8_default
!     v1 = 0.5_default !!! NLO v1
!     v2 = 0.8_default
!     v1 = 0.45_default !!! NLO v2
!     v2 = 0.65_default
!     v1 = 0.35_default !!! NLO v3
!     v2 = 0.65_default
!     v1 = 0.5_default !!! NLO v4
!     v2 = 0.65_default
!     v1 = 0.45_default !!! NLO v5
!     v2 = 0.8_default
!     v1 = 0.4_default !!! NLO v6
!     v2 = 0.75_default
!     v1 = 0.55_default !!! NLO v7
!     v2 = 0.8_default
    f1 = 1.0_default
    f2 = 0.0_default
    vm = (v1+v2) / 2.
    if ( v < v1 ) then
      fval = f1
    else if ( v < vm ) then
      fval = (f2-f1) / ((vm-v1)*(v2-v1)) * (v-v1)**2 + f1
    else if ( v < v2 ) then
      fval = (f2-f1) / ((vm-v2)*(v2-v1)) * (v-v2)**2 + f2
    else
      fval = f2
    end if
!    fval = 0.0_default
  end function f_matching

  !!! actual matching procedures:
  subroutine match_resummed_formfactor (ff, ps, i)
    complex(default), intent(inout) :: ff
    type(phase_space_point_t), intent(in) :: ps
    integer, intent(in) :: i
    select case (matching_version)
      case (0)
        return
      case (1)
        call match_resummed_formfactor_Andre (ff, ps, i, match_to_NLO)
      case (2)
        call match_resummed_formfactor_Max (ff, ps, i, match_to_NLO)
      case (3)
        call match_resummed_formfactor_Andre (ff, ps, i, match_to_NLO)
      case (4)
        call match_resummed_formfactor_Max (ff, ps, i, match_to_NLO)
      case default
        call msg_fatal ("match_resummed_formfactor: invalid matching_version = " &
                          // char(matching_version))
    end select
  end subroutine match_resummed_formfactor

  !!! Max's proposal
  subroutine match_resummed_formfactor_Max (ff, ps, i, NLO_flag)
    complex(default), intent(inout) :: ff
    type(phase_space_point_t), intent(in) :: ps
    integer, intent(in) :: i
    logical, intent(in) :: NLO_flag
    real(default) :: fm, as_f
    complex(default) :: FFnr_exp_as, R
    fm = f_matching (v_matching (ps))
    as_f = alphas_soft (ps%sqrts, nloop, fm*ah)
    FFnr_exp_as = nonrelativistic_formfactor (as, ps, i)
    if ( NLO_flag ) then
      R = ( ff - FFnr_exp_as ) / ( formfactor_LL_analytic (as, ps, i) - FFnr_exp_as )
      ff = nonrelativistic_formfactor ((as-ah), ps, i) + &
           R * (   formfactor_LL_analytic (as_f, ps, i) &
                 - nonrelativistic_formfactor (as_f, ps, i) )
    else
      R = ( ff - one ) / ( formfactor_LL_analytic (as, ps, i) - FFnr_exp_as )
      ff = one + R * (   formfactor_LL_analytic (as_f, ps, i) &
                       - nonrelativistic_formfactor (as_f, ps, i) )
    end if
  end subroutine match_resummed_formfactor_Max

  !!! Andre's proposal
  subroutine match_resummed_formfactor_Andre (ff, ps, i, NLO_flag)
    complex(default), intent(inout) :: ff
    type(phase_space_point_t), intent(in) :: ps
    integer, intent(in) :: i
    logical, intent(in) :: NLO_flag
    real(default) :: fm, as_f
    complex(default) :: FFnr_resum_f
    fm = f_matching (v_matching (ps))
    as_f = alphas_soft (ps%sqrts, nloop, fm*ah)
    FFnr_resum_f = formfactor_LL_analytic (as_f, ps, i)
    if ( nloop > 0 ) FFnr_resum_f = FFnr_resum_f + &
      fm**2 * ( ff - formfactor_LL_analytic (as, ps, i) )
    if ( NLO_flag ) then
      ff = nonrelativistic_formfactor ((as-ah), ps, i) + &
           FFnr_resum_f - nonrelativistic_formfactor (as_f, ps, i)
    else
      ff = FFnr_resum_f
    end if
  end subroutine match_resummed_formfactor_Andre

  function formfactor_LL_analytic (a_soft, sqrts, p, i) result (c)
    real(default), intent(in) :: a_soft
    real(default), intent(in) :: sqrts
    real(default), intent(in) :: p
    integer, intent(in) :: i
    complex(default) :: c
    real(default) :: en
    c = one
    if ( .not.init_pars ) return
    en = sqrts_to_en (sqrts, mtpole)
    select case (i)
      case (1)
        c = G0p (CF*a_soft, en, p, mtpole, gam) / G0p_tree (en, p, mtpole, gam)
      case (2)
        c = G0p_ax (CF*a_soft, en, p, mtpole, gam) / G0p_tree (en, p, mtpole, gam)
      case default
        call msg_fatal ("unknown ttZ/ttA vertex component i = " // char(i))
    end select
  end function formfactor_LL_analytic

  !!! Max's LL nonrelativistic threshold Green's function
  function G0p (a, en, p, m, w) result (c)
    real(default), intent(in) :: a
    real(default), intent(in) :: en
    real(default), intent(in) :: p
    real(default), intent(in) :: m
    real(default), intent(in) :: w
    complex(default) :: c
    complex(default) :: k, ipk, la, z1, z2
    complex(default) :: one, two, cc, dd
    k   = sqrt( -m*en -imago*m*w )
    ipk = imago * p / k
    la  = a * m / 2. / k
    one = cmplx (1., kind=default)
    two = cmplx (2., kind=default)
    cc  = 2. - la
    dd  = ( 1. + ipk ) / 2.
    z1  = nr_hypgeo (two, one, cc, dd)
    dd  = ( 1. - ipk ) / 2.
    z2  = nr_hypgeo (two, one, cc, dd)
    c   = - imago * m / (4.*p*k) / (1.-la) * ( z1 - z2 )
  end function G0p

  !!! tree level version: a_soft -> 0
  pure function G0p_tree (en, p, m, w) result (c)
    real(default), intent(in) :: en
    real(default), intent(in) :: p
    real(default), intent(in) :: m
    real(default), intent(in) :: w
    complex(default) :: c
    c = m / (p**2 - m*(en+imago*w))
  end function G0p_tree

  !!! Peter Poier's LL nonrelativistic axial threshold Green's function
  function G0p_ax (a, en, p, m, w) result (c)
    real(default), intent(in) :: a
    real(default), intent(in) :: en
    real(default), intent(in) :: p
    real(default), intent(in) :: m
    real(default), intent(in) :: w
    complex(default) :: c
    complex(default) :: k, ipk, la, z1, z2, z3, z4
    complex(default) :: zero, two, three, cc, ddp, ddm
    k   = sqrt( -m*en -imago*m*w )
    ipk = imago * p / k
    la  = a * m / 2. / k
    zero = cmplx (0., kind=default)
    two = cmplx (2., kind=default)
    three = cmplx (3., kind=default)
    cc  = 1. - la
    ddp = ( 1. + ipk ) / 2.
    ddm = ( 1. - ipk ) / 2.
    z1  = nr_hypgeo (zero, two, cc, ddp)
    z2  = nr_hypgeo (zero, two, cc, ddm)
    cc  = 2. - la
    z3  = nr_hypgeo (zero, three, cc, ddm)
    z4  = nr_hypgeo (zero, three, cc, ddp)
    c   = m / 2. / p**3 * ( 2.*p + imago*k*(1.-la)*(z1-z2) + imago*k*(z3-z4) )
  end function G0p_ax

  !!! include |p0| dependence
  function formfactor_LL_analytic_p0 (a_soft, ps, i) result (c)
    real(default), intent(in) :: a_soft
    type(phase_space_point_t), intent(in) :: ps
    integer, intent(in) :: i
    complex(default) :: c
    c = one
    if ( .not.init_pars ) return
    select case (i)
      case (1)
        c = formfactor_LL_analytic_p0_swave (CF*a_soft, ps%en, ps%p, ps%p0, ps%mpole, gam)
      case (2)
        !!! not implemented yet
!        c = formfactor_LL_analytic_p0_pwave (CF*a_soft, en, p, p0, mtpole, gam)
        c = one
      case default
        call msg_fatal ("unknown ttZ/ttA vertex component i = " // char(i))
    end select
  end function formfactor_LL_analytic_p0

  function formfactor_LL_analytic_p0_swave (a, en, p, p0, m, w) result (c)
    real(default), intent(in) :: a
    real(default), intent(in) :: en
    real(default), intent(in) :: p
    real(default), intent(in) :: p0
    real(default), intent(in) :: m
    real(default), intent(in) :: w
    complex(default) :: c
    complex(default) :: k, la, z1, z2
    complex(default) :: aa, bb, cc, dd
    real(default) :: eps = 1.E-3
    k  = sqrt( -m*en -imago*m*w )
    la = a * m / 2. / k
    aa = eps
    bb = 1.+eps
    cc = 1. + eps - la
    dd = (k-imago*(p-abs(p0))) / (2.*k)
    z1 = nr_hypgeo (aa, bb, cc, dd)
    dd = (k+imago*(p+abs(p0))) / (2.*k)
    z2 = nr_hypgeo (aa, bb, cc, dd)
    !!! DGamma(x) is Fortran 2008: use NR's implementation
    c  = one - imago*k*la * &
           nr_gamma(eps) * nr_gamma(1.+eps) * nr_gamma(real(1.-la)) * &
           ( -z1 + z2 ) / (p*nr_gamma(real(cc)))
  end function formfactor_LL_analytic_p0_swave

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

  pure function alphas_soft (sqrts, nl, a_hard_in) result (a_soft)
    real(default), intent(in) :: sqrts
    integer, intent(in) :: nl
    real(default), intent(in), optional :: a_hard_in
    real(default) :: a_soft
    real(default) :: mu_soft, a_hard
    mu_soft = m1s * h * f * nustar(sqrts)
    a_hard = ah
    if ( present(a_hard_in) ) a_hard = a_hard_in
    a_soft = running_as (mu_soft, a_hard, mu_h, nl, nf)
  end function alphas_soft

  pure function m1s_to_mpole (sqrts, nl_in) result (mpole)
    real(default), intent(in) :: sqrts
    integer, intent(in), optional :: nl_in
    real(default) :: mpole
    integer :: nl
    mpole = mtpole_init
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
    flag = ( sqrts >= 2.*m1s+sqrts_lo-tiny_07 .and. sqrts <= 2.*m1s+sqrts_hi+tiny_07 )
  end function

  pure function sqrts_iter (i_sq) result (sqrts)
    integer, intent(in) :: i_sq
    real(default) :: sqrts
    sqrts = 2.*m1s + sqrts_lo - sqrts_it + &
            (sqrts_hi-sqrts_lo+2.*sqrts_it) * real(i_sq-1) / real(n_sq-1)
  end function sqrts_iter

  ! TODO: (bcn 2015-07-31) this is unstable for small b. Take nearly_equal instead
  pure function is_equal (a, b) result (flag)
    real(default), intent(in) :: a
    real(default), intent(in) :: b
    logical :: flag
    real(single) :: val, acc
    acc = 1.e-6
    val = abs( a/b - 1.0_single )
    flag = ( val < acc )
  end function is_equal

  function scan_formfactor_over_p_LL_analytic (a_soft, sqrts, i) result (ff_analytic)
    real(default), intent(in) :: a_soft
    real(default), intent(in) :: sqrts
    integer, intent(in) :: i
    complex(default), dimension(n_p) :: ff_analytic
    integer :: i_p
    ff_analytic = (/ (formfactor_LL_analytic (a_soft, sqrts, p_grid(i_p), i), i_p=1, n_p) /)
    if ( need_p0 ) call ff_p_spline%init (p_grid, ff_analytic)
  end function scan_formfactor_over_p_LL_analytic

  !!! tttoppik wrapper
  function scan_formfactor_over_p_TOPPIK (a_soft, sqrts, i, p_grid_out, mpole_in) result (ff_toppik)
    real(default), intent(in) :: a_soft
    real(default), intent(in) :: sqrts
    integer, intent(in) :: i
    real(default), dimension(n_p), intent(out), optional :: p_grid_out
    real(default), intent(in), optional :: mpole_in
    complex(default), dimension(n_p) :: ff_toppik
    integer :: i_p
    real(default) :: mpole
    real(default), dimension(n_p) :: p_toppik
    type(nr_spline_t) :: toppik_spline

    real*8 :: xenergy, xtm, xtg, xalphas, xscale, xc0, xc1, xc2, xim, xdi, &
        xcutn, xcutv, xkincm, xkinca, xkincv, xcdeltc, &
        xcdeltl, xcfullc, xcfulll, xcrm2
    integer, parameter :: nmax=400
    real*8 :: xdsdp(nmax), xpp(nmax), xww(nmax)
    complex*16 :: zff(nmax)
    integer :: np, jknflg, jgcflg, jvflg

    if ( n_p > nmax-40 ) call msg_fatal ("TOPPIK: n_p must be <=" // char(nmax-40))
    ff_toppik = (/ (0.0_default, i_p=1, n_p) /)
    mpole = mtpole
    if ( present(mpole_in) ) mpole = mpole_in

    xenergy = sqrts_to_en (sqrts, mtpole)
    xtm     = mpole
    xtg     = gam
    xalphas = a_soft
    xscale  = mu_s
    xcutn   = 175.E6
    xcutv   = 175.E6
    xc0     = xc (a_soft, 0)
    xc1     = xc (a_soft, 1)
    xc2     = xc (a_soft, 2)
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
                jgcflg, xkincv,jvflg,xim,xdi,np,xpp,xww,xdsdp,zff)
      case (2)
        call tttoppikaxial &
               (xenergy,xtm,xtg,xalphas,xscale,xcutn,xcutv,xc0,xc1,xc2, &
                xcdeltc,xcdeltl,xcfullc,xcfulll,xcrm2,xkincm,xkinca,jknflg, &
                jgcflg, xkincv,jvflg,xim,xdi,np,xpp,xww,xdsdp,zff)
        !!! 1st ~10 TOPPIK p-wave entries are ff_unstable: discard them
        zff(1:10) = (/ (zff(11), i_p=1, 10) /)
      case default
        call msg_fatal ("unknown ttZ/ttA vertex component i = " // char(i))
    end select

    if ( present(p_grid_out) ) then
      p_grid_out = xpp(1:n_p)
      return
    end if

    !!! keep track of TOPPIK instabilities and try to repair later
    if ( np < 0 ) then
      ff_toppik(1) = 2.d30
      if (debug_active (D_THRESHOLD)) then
         call msg_warning ("caught TOPPIK instability at sqrts = " // char(sqrts))
      end if
      return
    end if
    p_toppik = xpp(1:n_p)
    ff_toppik = zff(1:n_p)
    if ( need_p0 ) then
      call ff_p_spline%init (p_toppik, ff_toppik)
    else
      !!! TOPPIK output p-grid scales with en above ~ 4 GeV:
      !!! interpolate for global sqrts/p grid
      if (.not. is_equal (p_toppik(42), p_grid(42))) then
        call toppik_spline%init (p_toppik, ff_toppik)
        ff_toppik(2:n_p) = [(toppik_spline%interpolate (p_grid(i_p)), i_p=2, n_p)]
        call toppik_spline%dealloc ()
      end if
      !!! TOPPIK output includes tree level ~ 1, a_soft @ LL in current coefficient!
      ff_toppik = ff_toppik * current_coeff (ah, asLL, au, i)
    end if
  end function scan_formfactor_over_p_TOPPIK

  function scan_formfactor_over_p (a_soft, sqrts, i) result (ff)
    real(default), intent(in) :: a_soft
    real(default), intent(in) :: sqrts
    integer, intent(in) :: i
    complex(default), dimension(n_p) :: ff
    select case (nloop)
      case (0)
!        ff = scan_formfactor_over_p_LL_analytic (a_soft, i, i_sq)
        ff = scan_formfactor_over_p_TOPPIK (a_soft, sqrts, i)
      case (1)
        ff = scan_formfactor_over_p_TOPPIK (a_soft, sqrts, i)
      case default
        call msg_fatal ("nloop = " // char(nloop))
    end select
  end function scan_formfactor_over_p

  subroutine scan_formfactor_over_phase_space_grid ()
    real(default) :: a_soft
    integer :: i_sq, i
    logical, dimension(:,:), allocatable :: ff_unstable
    real(default) :: t1, t2, t3, t_toppik, t_p0_dep
    call msg_debug (D_THRESHOLD, "scan_formfactor_over_phase_space_grid")
    allocate (ff_grid(n_sq,n_p,n_p0,2))
    allocate (ff_unstable(n_sq,2))
    t_toppik = zero
    t_p0_dep = zero
    call msg_message ("Scanning over requested energy range")
    ENERGY_SCAN: do i_sq = 1, n_sq
      call update_soft_parameters (sq_grid(i_sq))
      a_soft = as
      !!! vector and axial vector
      do i = 1, 2
        call cpu_time (t1)
        UNTIL_STABLE: do
           ff_grid(i_sq,:,1,i) = scan_formfactor_over_p (a_soft, sq_grid(i_sq), i)
           ff_unstable(i_sq,i) = abs(ff_grid(i_sq,1,1,i)) > 1.d30
           if (ff_unstable(i_sq,i))  cycle
           exit
        end do UNTIL_STABLE
        call cpu_time (t2)
        !!!  include p0 dependence by an integration over the p0-independent FF
        if (need_p0)  ff_grid(i_sq,1:n_p_p0dep,:,i) = &
             scan_formfactor_over_p_p0 (a_soft, sq_grid(i_sq), i)
        call cpu_time (t3)
        t_toppik = t_toppik + t2 - t1
        t_p0_dep = t_p0_dep + t3 - t2
        if (need_p0)  call ff_p_spline%dealloc ()
      end do
      call msg_show_progress (i_sq, n_sq)
    end do ENERGY_SCAN
    if (debug_active (D_THRESHOLD)) then
       print *, "time for TOPPIK call:   ", t2 - t1, " seconds."
       print *, "time for p0 dependence: ", t3 - t2, " seconds."
    end if
    if (any (ff_unstable))  call handle_TOPPIK_instabilities (ff_grid, ff_unstable)
    deallocate (ff_unstable)
    if (allocated(Vmatrix))  deallocate(Vmatrix)
    if (allocated(q_grid))  deallocate(q_grid)
    if (need_p0)  call trim_p_grid (n_p_p0dep)
    init_ff = .true.
  end subroutine scan_formfactor_over_phase_space_grid

  subroutine init_threshold_phase_space_grid ()
    integer :: i_sq
    call msg_debug (D_THRESHOLD, "init_threshold_phase_space_grid")
    n_sq = int ((sqrts_hi - sqrts_lo) / sqrts_it + tiny_07) + 1 + 2
    call msg_debug (D_THRESHOLD, "Number of sqrts grid points: n_sq", n_sq)
    call msg_debug (D_THRESHOLD, "sqrts_hi", sqrts_hi)
    call msg_debug (D_THRESHOLD, "sqrts_lo", sqrts_lo)
    call msg_debug (D_THRESHOLD, "sqrts_it", sqrts_it)
    allocate (sq_grid(n_sq))
    sq_grid = [(sqrts_iter (i_sq), i_sq=1, n_sq)]
    n_p = 360
    allocate (p_grid(n_p))
    p_grid = p_grid_from_TOPPIK ()
    if (need_p0) then
      if (ff_type == 0 .and. ext_Vinput) then
        ! TODO: (bcn 2015-07-30) why is mpole hard coded here?
        p_grid = p_grid_from_TOPPIK (173.0_default)
        call import_Vmatrices ()
      else
        n_p0 = 85
        n_p_p0dep = 315
      end if
      call init_p0_grid (p_grid, n_p0)
    else
      n_p0 = 1
    end if
    if (need_J0) call finer_grid (p_grid(1:n_p_p0dep), p_grid_fine)
    init_ps = .true.
  end subroutine init_threshold_phase_space_grid

  subroutine init_p0_grid (p_in, n)
    real(default), dimension(:), allocatable, intent(in) :: p_in
    integer, intent(in) :: n
    call msg_debug (D_THRESHOLD, "init_p0_grid")
    call msg_debug (D_THRESHOLD, "n", n)
    call msg_debug (D_THRESHOLD, "size(p_in)", size(p_in))
    if (.not. allocated (p_in))  call msg_fatal ("init_p0_grid: p_in not allocated!")
    if (allocated (p0_grid))  deallocate (p0_grid)
    allocate (p0_grid(n))
    p0_grid(1) = 0.0_default
    p0_grid(2:n) = p_in(1:n-1)
  end subroutine init_p0_grid

  !!! Andre's procedure to refine an existing grid
  pure subroutine finer_grid (gr, fgr, n_in)
    real(default), dimension(:), intent(in) :: gr
    real(default), dimension(:), allocatable, intent(inout) :: fgr
    integer, intent(in), optional :: n_in
    integer :: n, i, j
    real(default), dimension(:), allocatable :: igr
    n = 4
    if ( present(n_in) ) n = n_in
    allocate( igr(n) )
    if ( allocated(fgr) ) deallocate( fgr )
    allocate( fgr(n*(size(gr)-1)+1) )
    do i=1, size(gr)-1
      do j=0, n-1
        igr(j+1) = gr(i) + real(j)*(gr(i+1)-gr(i))/real(n)
      end do
      fgr((i-1)*n+1:i*n) = igr
    end do
    fgr(size(fgr)) = gr(size(gr))
    deallocate( igr )
  end subroutine finer_grid

  subroutine dealloc_grids ()
    if ( allocated(sq_grid) ) deallocate( sq_grid )
    if ( allocated( p_grid) ) deallocate(  p_grid )
    if ( allocated(p0_grid) ) deallocate( p0_grid )
    if ( allocated(ff_grid) ) deallocate( ff_grid )
    if ( allocated(J0_grid) ) deallocate( J0_grid )
    init_ps = .false.
    init_ff = .false.
    init_J0 = .false.
  end subroutine dealloc_grids

  subroutine trim_p_grid (n_p_new)
    integer, intent(in) :: n_p_new
    real(default), dimension(n_p_new) :: p_save
    complex(default), dimension(n_sq,n_p_new,n_p0,2) :: ff_save
    if ( n_p_new > n_p ) then
      call msg_fatal ("trim_p_grid: new size larger than old size.")
      return
    end if
    p_save = p_grid(1:n_p_new)
    ff_save = ff_grid(:,1:n_p_new,:,:)
    deallocate( p_grid, ff_grid )
    allocate( p_grid(n_p_new), ff_grid(n_sq,n_p_new,n_p0,2) )
    p_grid = p_save
    ff_grid = ff_save
  end subroutine trim_p_grid

  !!! try to repair TOPPIK instabilities by interpolation of adjacent sq_grid points
  subroutine handle_TOPPIK_instabilities (ff, nan)
    complex(default), dimension(:,:,:,:), intent(inout) :: ff
    logical, dimension(:,:), intent(in) :: nan
    integer :: i, i_sq, n_nan
    logical :: interrupt
    n_nan = sum( merge((/(1,i=1,2*n_sq)/), (/(0,i=1,2*n_sq)/), reshape(nan,(/2*n_sq/))) )
    interrupt = ( n_nan > 3 )
    do i=1, 2
      if ( interrupt ) exit
      if ( .not.any(nan(:,i)) ) cycle
      do i_sq=2, n_sq-1
        if ( .not.nan(i_sq,i) ) cycle
        if ( nan(i_sq+1,i) .or. nan(i_sq-1,i) ) then
          interrupt = .true.
          exit
        end if
        ff(i_sq,:,:,i) = ( ff(i_sq-1,:,:,i) + ff(i_sq+1,:,:,i) ) / 2.
      end do
    end do
    if ( .not.interrupt ) return
    call msg_fatal ("Too many TOPPIK instabilities! Check your parameter setup " &
                     // "or slightly vary the scales sh and/or sf.")
  end subroutine handle_TOPPIK_instabilities

  pure function sqrts_to_v (sqrts) result (v)
    real(default), intent(in) :: sqrts
    complex(default) :: v
    real(default) :: m
    m = m1s_to_mpole (sqrts)
    v = sqrt( (sqrts - 2.*m + imago*gam) / m )
  end function sqrts_to_v

  pure function v_to_sqrts (v) result (sqrts)
    real(default), intent(in) :: v
    real(default) :: sqrts
    real(default) :: m
    m = mtpole_init
    sqrts = 2.*m + m*v**2
  end function v_to_sqrts

  !!! convert squared 4-momenta into sqrts, p0 = E_top-sqrts/2 and abs. 3-momentum p
  pure subroutine rel_to_nonrel (p2, k2, q2, sqrts, p, p0)
    real(default), intent(in) :: p2
    real(default), intent(in) :: k2
    real(default), intent(in) :: q2
    real(default), intent(out) :: sqrts
    real(default), intent(out) :: p
    real(default), intent(out) :: p0
    sqrts = sqrt(q2)
    p0 = abs(p2 - k2) / (2.*sqrts)
    p = sqrt( 0.5_default*(-p2 - k2 + sqrts**2/2. + 2.*p0**2) )
  end subroutine rel_to_nonrel

  !!! convert sqrts, p0 = E_top-sqrts/2 and abs. 3-momentum p into squared 4-momenta
  pure subroutine nonrel_to_rel (sqrts, p, p0, p2, k2, q2, m2)
    real(default), intent(in) :: sqrts
    real(default), intent(in) :: p
    real(default), intent(in) :: p0
    real(default), intent(out) :: p2
    real(default), intent(out) :: k2
    real(default), intent(out) :: q2
    complex(default), intent(out), optional :: m2
    p2 = (sqrts/2.+p0)**2 - p**2
    k2 = (sqrts/2.-p0)**2 - p**2
    q2 = sqrts**2
    if ( present(m2) ) m2 = complex_m2 (m1s_to_mpole(sqrts), gam)
  end subroutine nonrel_to_rel

  pure function complex_m2 (m, w) result (m2c)
    real(default), intent(in) :: m
    real(default), intent(in) :: w
    complex(default) :: m2c
    m2c = m**2 - imago*m*w
  end function complex_m2

  pure function total_top_width_LO (alphaem, sw, Vtb, mt, mW, mb) result (wt)
    real(default), intent(in) :: alphaem
    real(default), intent(in) :: sw
    real(default), intent(in) :: Vtb
    real(default), intent(in) :: mt
    real(default), intent(in) :: mW
    real(default), intent(in) :: mb
    real(default) :: wt
    wt = alphaem/4. * mt/(2.*sw**2) * Vtb**2 * ((mt**2 + mb**2)/(2.*mt**2) + &
         (mt**2 - mb**2)**2/(2.*mt**2*mW**2) - mW**2/mt**2) / mt**2 * &
         sqrt((mt**2 - (mW + mb)**2) * (mt**2 - (mW - mb)**2))
  end function total_top_width_LO

  !!! -q^2 times the Coulomb potential V at LO resp. NLO
  function minus_q2_V (a, q, p, p0r, i) result (v)
    real(default), intent(in) :: a
    real(default), intent(in) :: q
    real(default), intent(in) :: p
    real(default), intent(in) :: p0r
    integer, intent(in) :: i
    complex(default) :: p0, log_mppp, log_mmpm, log_mu_s, v
    p0 = abs(p0r) + ieps
    log_mppp = log( (p-p0+q) * (p+p0+q) )
    log_mmpm = log( (p-p0-q) * (p+p0-q) )
    select case (i)
      case (1)
        select case (nloop)
          case (0)
            v = CF*a * 2.*pi*(log_mppp-log_mmpm) * q/p
          case (1)
            log_mu_s = 2.*log(mu_s)
            v = CF*a * ( 2.*(4.*pi+a1*a)*(log_mppp-log_mmpm) &
                      +b0*a*((log_mmpm-log_mu_s)**2-(log_mppp-log_mu_s)**2) ) * q/(4.*p)
          case default
            call msg_fatal ("nloop = " // char(nloop))
        end select
      case (2)
        !!! not implemented yet
        v = 0.0_default
      case default
        call msg_fatal ("unknown ttZ/ttA vertex component i = " // char(i))
    end select
  end function minus_q2_V

  function scan_formfactor_over_p_p0 (a_soft, sqrts, i) result (ff_p0)
    real(default), intent(in) :: a_soft
    real(default), intent(in) :: sqrts
    complex(default), dimension(n_p_p0dep,n_p0) :: ff_p0
    integer, intent(in) :: i
    complex(single), dimension(:,:,:), allocatable :: Vmat
    complex(default), dimension(:), allocatable :: Tvec
    integer :: i_p, i_p0, i_q
    real(default) :: en, p, p0
    type(phase_space_point_t) :: ps
    type(p0_q_integrand_t) :: q_integrand
    complex(default) :: q_integral, ff
    real(default) :: current_c1
    if (i==2) return
    en = sqrts_to_en (sqrts, mtpole)
    if (ext_Vinput) then
      allocate (Vmat(n_p0,n_p_p0dep,n_q))
      allocate (Tvec(n_q))
      select case (nloop)
         case (0)
           Vmat = Vmatrix(0,i,:,:,:) * a_soft
         case (1)
           Vmat = Vmatrix(0,i,:,:,:) * (a_soft + a_soft**2 *b0*log(mu_s)/(2*pi)) + &
                  Vmatrix(1,i,:,:,:) * a_soft**2
         case default
           call msg_fatal ("nloop = " // char(nloop))
      end select
      do i_q = 1, n_q
         Tvec(i_q) = ff_p_spline%interpolate(q_grid(i_q)) * &
              G0p_tree(en,q_grid(i_q),mtpole,gam)
!         Tvec(i_q) = formfactor_LL_analytic (a_soft, sqrts, q_grid(i_q), i) * &
!                     G0p_tree(en,q_grid(i_q),mtpole,gam)
      end do
    end if
    !!! a_soft @ LL in current coefficient!
    current_c1 = current_coeff (ah, asLL, au, i)
    !!! integrate over q for each p, p0:
    do i_p = 1, n_p_p0dep
       p = p_grid(i_p)
       do i_p0 = 1, n_p0
          p0 = p0_grid(i_p0)
          call ps%init_nonrel (sqrts, p, p0)
          if (ext_Vinput) then
             !!! Andre's matrix summation
             q_integral = sum (Vmat(i_p0,i_p,:) * Tvec)
          else if (nloop > 0) then
             !!! numerical integration using NR's Gaussian summation
             call compute_support_points (en, i_p, i_p0, 10)
!             q_integral = 1./(2.*pi)**2 * nr_qgaus (integrand, q_grid)
             call q_integrand%update (a_soft, ps, i)
             q_integral = 1./(2.*pi)**2 * solve_qgaus (q_integrand, q_grid)
          else
             !!! analytic FF incl. p0 dependence @ LL
             q_integral = formfactor_LL_analytic (a_soft, ps, i) - one
          end if
          !!! q_integral is a pure correction of O(alphas): add tree level ~ 1 again
          ff = current_c1 * (one + q_integral)
          if (matching_version > 0)  call match_resummed_formfactor (ff, ps, i)
          ff_p0(i_p,i_p0) = ff
       end do
    end do
    if (ext_Vinput) then
       deallocate(Vmat)
       deallocate(Tvec)
    end if
  end function scan_formfactor_over_p_p0

  !!! compute support points (~> q-grid) for numerical integration: trim p-grid and
  !!! merge with singular points of integrand: q = p, |p-p0|, p+p0, sqrt(mpole*E)
  subroutine compute_support_points (en, i_p, i_p0, n_trim)
    real(default), intent(in) :: en
    integer, intent(in) :: i_p
    integer, intent(in) :: i_p0
    integer, intent(in) :: n_trim
    real(default) :: p, p0
    real(default), dimension(4) :: sing_vals
    integer :: n_sing, i_q
    if ( mod(n_p,n_trim) /= 0 ) call msg_fatal ("trim p-grid for q-integration: n_p = " &
                                  // char(n_p) // " and n_trim = " // char(n_trim))
    n_q = n_p/n_trim + merge(0,1,n_trim==1)
    p = p_grid(i_p)
    p0 = p0_grid(i_p0)
    n_sing = 0
    if ( i_p /= 1 .and. mod(i_p,n_trim) /= 0 ) then
      n_sing = n_sing+1
      sing_vals(n_sing) = p
    end if
    if ( i_p0 /= 1 ) then
      n_sing = n_sing+1
      sing_vals(n_sing) = p0 + p
      if ( i_p0 /= i_p+1 ) then
        n_sing = n_sing+1
        sing_vals(n_sing) = abs( p0 - p )
      end if
    end if
    if ( en > 0. ) then
      n_sing = n_sing+1
      sing_vals(n_sing) = sqrt( mtpole * en )
    end if
    if ( allocated(q_grid) ) deallocate( q_grid )
    allocate( q_grid(n_q+n_sing) )
    q_grid(1) = p_grid(1)
    q_grid(2:n_q) = (/ (p_grid(i_q), i_q=max(n_trim,2), n_p, n_trim) /)
    if (n_sing > 0 ) q_grid(n_q+1:n_q+n_sing) = sing_vals(1:n_sing)
    call nr_sort (q_grid)
  end subroutine compute_support_points

  subroutine import_Vmatrices ()
    complex(single), dimension(:), allocatable :: mat_1d
    logical :: ex
    integer :: u, st, i_line, i_loop, i
    character(len=1) :: flag
    real(single) :: re, im
    type(string_t) :: Vpath, Vfile
    Vpath = PREFIX // "/share/whizard/SM_tt_threshold_data/SM_tt_threshold_Vmatrices/"
    do i = 1, 2
       ! TODO: (bcn 2015-07-31) I suppose this should be removed at some point?!
       if (i==2) return
       do i_loop = 0, nloop
          select case (10*i+i_loop)
             case (10)
               Vfile = Vpath // "Vmatrix_s-wave_LO.dat"
             case (11)
               Vfile = Vpath // "Vmatrix_s-wave_NLO.dat"
             case (20)
               Vfile = Vpath // "Vmatrix_p-wave_LO.dat"
             case (21)
               Vfile = Vpath // "Vmatrix_p-wave_NLO.dat"
             case default
               call msg_fatal ("import Vmatrix: no input file for i_loop = "  &
                                 // char(i_loop) // " and i = " // char(i))
          end select
          inquire (file=char(Vfile), exist=ex)
          call msg_message ("Trying to load " // char(Vfile))
          if (.not.ex) then
             call msg_message ("Input data missing. You may choose to:")
             call msg_message (" (d)ownload files from whizard.hepforge.org (180/590 MB packed/unpacked);")
             call msg_message (" (c)ompute data on the fly (may take 1-3 hours to initialize).")
             call msg_message (" Please enter d/c:")
             read (input_unit, *) flag
             select case (flag)
               case ("d")
                 call msg_message ("===> Please run the download script:")
                 call msg_message (PREFIX // "/share/whizard/SM_tt_threshold_data/download_data.sh")
                 call msg_message ("and restart WHIZARD.")
                 call msg_terminate ()
               case ("c")
                 ext_Vinput = .false.
                 return
               case default
                 call msg_fatal ("unknown option " // flag)
             end select
          end if
          i_line = 1
          u = free_unit ()
          open (u, file=char(Vfile), status='old', action='read', iostat=st)
          if (st /= 0) call msg_fatal ("open " // char(Vfile) // ": iostat = " // char(st))
          PARSE: do
             if (i_line == 1) then
                read (u, *, iostat=st) n_p0, n_p_p0dep, n_q
                if (st /= 0) exit PARSE
                if (.not. allocated (q_grid)) allocate (q_grid(n_q))
                allocate (mat_1d(n_p0*n_p_p0dep*n_q))
             else if (i_line <= n_q+1) then
                read (u, *, iostat=st) q_grid(i_line-1)
                if (st /= 0)  exit PARSE
             else
                read (u, *, iostat=st) re, im
                if (st /= 0)  exit PARSE
                mat_1d(i_line-n_q-1) = cmplx (re, im, kind=single)
             end if
             i_line = i_line + 1
          end do PARSE
          if (st > 0)  call msg_fatal ("import " // char(Vfile) // ": read line " &
                          // char(i_line) // ": iostat = " // char(st))
          close (u, iostat=st)
          if (st > 0)  call msg_fatal ("close " // char(Vfile) // ": iostat = " // char(st))
          if (i_line-n_q-2 /= size(mat_1d)) &
               call msg_fatal ("import Vmatrix: inconsistent input file " // char(Vfile))
          if (.not. allocated (Vmatrix)) then
             allocate (Vmatrix(0:nloop,2,n_p0,n_p_p0dep,n_q))
          else if (any ([n_p0,n_p_p0dep,n_q] /= shape (Vmatrix(0,1,:,:,:)))) then
             call msg_fatal ("import Vmatrix: incompatible shape in file " // char(Vfile))
          end if
          Vmatrix(i_loop,i,:,:,:) = reshape (mat_1d, (/ n_p0, n_p_p0dep, n_q /))
          deallocate (mat_1d)
       end do
    end do
  end subroutine import_Vmatrices

  !!! cf. arXiv:hep-ph/9503238, validated against arXiv:hep-ph/0008171
  pure function formfactor_ttv_relativistic_nlo (alphas, ps, J0) result (c)
    real(default), intent(in) :: alphas
    type(phase_space_point_t), intent(in) :: ps
    complex(default), intent(in) :: J0
    complex(default) :: c 
    real(default) :: p2, k2, q2, kp, pq, kq
    complex(default) :: D2, chi, ln1, ln2, L1, L2, z, S, m2, m
    complex(default) :: JA, JB, JC, JD, JE, IA, IB, IC, ID, IE
    complex(default) :: CCmsbar
    complex(default) :: dF1, dF2, dM1, dM2
    complex(default), dimension(12) :: P1
    p2 = ps%p2
    k2 = ps%k2
    q2 = ps%q2
    m2 = ps%m2
    !!! kinematic abbreviations
    kp = 0.5_default * (-q2 + p2 + k2)
    pq = 0.5_default * ( k2 - p2 - q2)
    kq = 0.5_default * (-p2 + k2 + q2)
    D2 = kp**2 - k2*p2
    chi = p2*k2*q2 + 2.*m2*((p2 + k2)*kp - 2.*p2*k2) + m2**2 * q2
    ln1 = log( (1. - p2/m2)*(1,0) + ieps )
    ln2 = log( (1. - k2/m2)*(1,0) + ieps )
    L1 = (1. - m2/p2) * ln1
    L2 = (1. - m2/k2) * ln2
    z = sqrt( (1.-4.*m2/q2)*(1,0) )
    S = 0.5_default * z * log( (z+1.)/(z-1.) + ieps )
    m = sqrt(m2)

    !!! loop integrals in terms of J0
    JA = 1./D2 * (J0/2.*(-m2*pq - p2*kq) + kp*L2 - p2*L1 - 2.*pq*S)
    JB = 1./D2 * (J0/2.*( m2*kq + k2*pq) + kp*L1 - k2*L2 + 2.*kq*S)
    JC = 1/(4.*D2) * (2.*p2 + 2*kp*m2/k2 - 4.*kp*S + 2.*kp*(1. - m2/k2)*L2 + &
            (2.*kp*(p2 - m2) + 3.*p2*(m2 - k2))*JA + p2*(m2 - p2)*JB)
    JD = 1./(4.*D2) * (2.*kp*((k2 - m2)*JA + (p2 - m2)*JB - 1.) - k2*(2.*m2/k2 &
            - 2.*S + (1. - m2/k2)*L2 + (p2 - m2)*JA) - p2*(-2.*S + (1. - &
            m2/p2)*L1 + (k2 - m2)*JB))
    JE = 1./(4.*D2) * (2.*k2 + 2*kp*m2/p2 - 4.*kp*S + 2.*kp*(1. - m2/p2)*L1 + &
            (2.*kp*(k2 - m2) + 3.*k2*(m2 - p2))*JB + k2*(m2 - k2)*JA)
    IA = 1./D2 * (-(kq/2.)*J0 - 2.*q2/chi *((m2 - p2)*k2 - (m2 - k2)*kp)*S + &
            1./(m2 - p2)*(p2 - kp + p2*q2/chi *(k2 - m2)*(m2 + kp))*L1 + &
            k2*q2/chi *(m2 + kp)*L2)
    IB = 1./D2 * ( (pq/2.)*J0 - 2.*q2/chi *((m2 - k2)*p2 - (m2 - p2)*kp)*S + &
            1./(m2 - k2)*(k2 - kp + k2*q2/chi *(p2 - m2)*(m2 + kp))*L2 + &
            p2*q2/chi *(m2 + kp)*L1)
    IC = 1./(4.*D2) * (2.*p2*J0 - 4.*kp/k2*(1. + m2/(k2 - m2)*L2) + (2.*kp - &
            3.*p2)*JA - p2*JB + (-2.*kp*(m2 - p2) + 3.*p2*(m2 - k2))*IA + &
            p2*(m2 - p2)*IB)
    ID = 1./(4.*D2) * (-2.*kp*J0 + 2.*(1. + m2/(k2 - m2)*L2) + 2.*(1. + &
            m2/(p2 - m2)*L1) + (2.*kp - k2)*JA + (2.*kp - p2)*JB + (k2*(m2 - &
            p2) - 2.*kp*(m2 - k2))*IA + (p2*(m2 - k2) - 2.*kp*(m2 - p2))*IB)
    IE = 1./(4.*D2) * (2.*k2*J0 - 4.*kp/p2*(1. + m2/(p2 - m2)*L1) + (2.*kp - &
            3.*k2)*JB - k2*JA + (-2.*kp*(m2 - k2) + 3.*k2*(m2 - p2))*IB + &
            k2*(m2 - k2)*IA)

    !!! divergent part ~ 1/epsilon: depends on subtraction scheme
    CCmsbar = -2.0_default * log(h)

    ! real top mass in the loop numerators
!    m2 = cmplx(real(m2), kind=default)
!    m  = sqrt(m2)

    !!! quark self energies
    dF1 = - (ximo+1.) * (CCmsbar + (1.+m2/p2)*(1.-L1))
    dF2 = - (ximo+1.) * (CCmsbar + (1.+m2/k2)*(1.-L2))
    dM1 = m/p2 * ( (ximo+1.)*(1.+m2/p2*ln1) - 3.*ln1 )
    dM2 = m/k2 * ( (ximo+1.)*(1.+m2/k2*ln2) - 3.*ln2 )

    !!! coefficient list: vertex function Gamma_mu (k,p) = sum_i( Vi_mu * Pi )
    P1(1)  =  2.*JA - 2.*JC + ximo*(m2*IC + p2*ID)
    P1(2)  =  2.*JB - 2.*JE + ximo*(k2*ID + m2*IE)
    P1(3)  = -2.*J0 + 2.*JA + 2.*JB - 2.*JD + ximo*(-J0/2. - k2/2.*IC - &
                 kp*ID + m2*ID + p2/2.*IE + JA)
    P1(4)  = -2.*JD + ximo*(k2*IC + m2*ID - JA)
    P1(5)  = J0 - JA - JB + ximo*(J0/4. + k2/4.*IC + kp/2.*ID + p2/4.*IE - &
                 1./2.*JA - 1./2.*JB)
    P1(6)  = -m2*J0 - k2*JA - p2*JB + k2/2.*JC + kp*JD + p2/2.*JE + &
                 (1./2. + CCmsbar - 2.*S) &
                 + ximo*(-m2*J0/4. - m2/4.*k2*IC - m2/2.*kp*ID - m2/4.*p2*IE &
                 - k2/2.*JA - p2/2.*JB + (CCmsbar + 2.))
    P1(7)  =  2.*m*J0 - 4.*m*JA + ximo*m*(J0/2. - 2.*kp*IC + k2/2.*IC - &
                 p2*ID - kp*ID - p2/2.*IE - JA)
    P1(8)  =  2.*m*J0 - 4.*m*JB + ximo*m*(J0/2. + k2/2.*IC - kp*ID + k2*ID - &
                 p2/2.*IE - JB)
    P1(9)  =  ximo*m*(ID + IE)
    P1(10) =  ximo*m*(ID + IC)
    P1(11) =  ximo*m*( p2*ID + kp*IC + p2/2.*IE - k2/2.*IC) + dM2
                                 !!! self energy contribution: ~ gamma_mu.k_slash = V11
    P1(12) =  ximo*m*(-k2*ID - kp*IE + p2/2.*IE - k2/2.*IC) + dM1
                                 !!! self energy contribution: ~ gamma_mu.p_slash = V12

    !!! leading form factor: V6 = gamma_mu, V5 = gamma_mu.k_slash.p_slash ~> -m^2*gamma_mu
    c = one + alphas * CF / (4.*pi) * ( P1(6) - m2*P1(5) &
                 !!! self energy contributions ~ gamma^mu
                 + dF1 + dF2 + m*( dM1 + dM2 ) )
                 !!! on-shell subtraction: UV divergence cancels
!                 + 0.5_default*( dF1 + dF2 + m*( dM1 + dM2 ) )
  end function formfactor_ttv_relativistic_nlo

  pure function p_onshell (en) result (p)
    real(default), intent(in) :: en
    real(default) :: p
    p = 1.d-3
    if ( en > 0. ) p = sqrt( en**2/4. + en*mtpole )
  end function p_onshell

  subroutine scan_J0_over_phase_space_grid ()
    integer :: i_sq, i_p, i_p0
    type(phase_space_point_t) :: ps
    complex(default) :: J0
    complex(default) :: J0_LoopTools
    external J0_LoopTools
    if (.not.init_ps)  call init_threshold_phase_space_grid ()
    if (.not.allocated(J0_grid))  allocate (J0_grid(n_sq,size(p_grid_fine),n_p0))
    do i_sq = 1, n_sq
      do i_p = 1, size(p_grid_fine)
        do i_p0 = 1, n_p0
          call ps%init_nonrel (sq_grid(i_sq), p_grid_fine(i_p), p0_grid(i_p0))
!          J0_grid(i_sq,i_p,i_p0) = J0_LoopTools (ps%p2, ps%k2, ps%q2, ps%m2)
          J0 = J0_LoopTools (ps%p2, ps%k2, ps%q2, ps%m2)
          J0_grid(i_sq,i_p,i_p0) = formfactor_ttv_relativistic_nlo (one, ps, J0) - one
        end do
      end do
    end do
    init_J0 = .true.
  end subroutine scan_J0_over_phase_space_grid

  pure function J0_LoopTools_interpolate (ps) result (J0)
    type(phase_space_point_t), intent(in) :: ps
    complex(default) :: J0
    J0 = 0.0_default
    if ( .not.init_J0 ) return
    if ( .not.ps%inside_grid ) return
    call interpolate_linear (sq_grid, p_grid_fine, p0_grid, J0_grid, &
                               ps%sqrts, ps%p, ps%p0, J0)
  end function J0_LoopTools_interpolate

  pure function sqrts_to_en (sqrts, mpole_in) result (en)
    real(default), intent(in) :: sqrts
    real(default), intent(in), optional :: mpole_in
    real(default) :: mpole, en
    if ( present(mpole_in) ) then
      mpole = mpole_in
    else
      mpole = m1s_to_mpole (sqrts)
    end if
    en = sqrts - 2.*mpole
  end function sqrts_to_en

  function p_grid_from_TOPPIK (mpole_in) result (p_toppik)
    real(default), intent(in), optional :: mpole_in
    real(default), dimension(n_p) :: p_toppik
    real(default) :: mpole
    complex(default), dimension(n_p) :: ff_dummy
    mpole = mtpole;  if (present (mpole_in))  mpole = mpole_in
    ff_dummy = scan_formfactor_over_p_TOPPIK &
                 (alphas_soft(2. * m1s, nloop), 2. * m1s, 1, p_toppik, mpole)
    if (.not. strictly_monotonous (p_toppik)) &
      call msg_fatal ("p_grid NOT strictly monotonous!")
  end function p_grid_from_TOPPIK

  pure function int_to_char (i) result (c)
    integer, intent(in) :: i
    character(len=len(trim(int2fixed(i)))) :: c
    c = int2char (i)
  end function int_to_char

  pure function real_to_char (r) result (c)
    real(default), intent(in) :: r
    character(len=len(trim(real2fixed(r)))) :: c
    c = real2char (r)
  end function real_to_char

  pure function complex_to_char (z) result (c)
    complex(default), intent(in) :: z
    character(len=len(trim(real2fixed(real(z))))+len(trim(real2fixed(aimag(z))))+5) :: c
    character(len=len(trim(real2fixed(real(z))))) :: re
    character(len=len(trim(real2fixed(aimag(z))))) :: im
    re = real_to_char (real(z))
    im = real_to_char (aimag(z))
    if ( is_equal(aimag(z), 0.0_default) ) then
      c = re
    else
      c = re // " + " // im // "*I"
    end if
  end function complex_to_char

  pure function logical_to_char (l) result (c)
    logical, intent(in) :: l
    character(len=1) :: c
    write (c, '(l1)') l
  end function logical_to_char

  subroutine p0_q_integrand_update (solver_f, a, ps, i)
    class(p0_q_integrand_t), intent(inout) :: solver_f
    real(default), intent(in) :: a
    type(phase_space_point_t), intent(in) :: ps
    integer, intent(in) :: i
    solver_f%a = a
    solver_f%ps = ps
    solver_f%i = i
  end subroutine p0_q_integrand_update

  function p0_q_integrand_evaluate (solver_f, x) result (f)
    complex(default) :: f
    class(p0_q_integrand_t), intent(in) :: solver_f
    real(default), intent(in) :: x
    f = G0p_tree (solver_f%ps%en, x, solver_f%ps%mpole, gam) &
          * minus_q2_V (solver_f%a, x, solver_f%ps%p, solver_f%ps%p0, solver_f%i) &
          * ff_p_spline%interpolate (x)
  end function p0_q_integrand_evaluate

  pure subroutine phase_space_point_init_rel (ps_point, p2, k2, q2, m)
    class(phase_space_point_t), intent(inout) :: ps_point
    real(default), intent(in) :: p2
    real(default), intent(in) :: k2
    real(default), intent(in) :: q2
    real(default), intent(in), optional :: m
    ps_point%p2 = p2
    ps_point%k2 = k2
    ps_point%q2 = q2
    call rel_to_nonrel (p2, k2, q2, ps_point%sqrts, ps_point%p, ps_point%p0)
    ps_point%mpole = m1s_to_mpole (ps_point%sqrts)
    ps_point%en = sqrts_to_en (ps_point%sqrts)
    ps_point%inside_grid = sqrts_within_range (ps_point%sqrts)
    ps_point%m2 = complex_m2 (ps_point%mpole, gam)
    if ( present(m) ) ps_point%onshell = ps_point%is_onshell (m)
  end subroutine phase_space_point_init_rel

  pure subroutine phase_space_point_init_nonrel (ps_point, sqrts, p, p0, m)
    class(phase_space_point_t), intent(inout) :: ps_point
    real(default), intent(in) :: sqrts
    real(default), intent(in) :: p
    real(default), intent(in) :: p0
    real(default), intent(in), optional :: m
    ps_point%sqrts = sqrts
    ps_point%p = p
    ps_point%p0 = p0
    call nonrel_to_rel (sqrts, p, p0, ps_point%p2, ps_point%k2, ps_point%q2)
    ps_point%mpole = m1s_to_mpole (sqrts)
    ps_point%en = sqrts_to_en (sqrts, ps_point%mpole)
    ps_point%inside_grid = sqrts_within_range (sqrts)
    ps_point%m2 = complex_m2 (ps_point%mpole, gam)
    if ( present(m) ) ps_point%onshell = ps_point%is_onshell (m)
  end subroutine phase_space_point_init_nonrel

  pure function phase_space_point_is_onshell (ps_point, m) result (flag)
    logical :: flag
    class(phase_space_point_t), intent(in) :: ps_point
    real(default), intent(in) :: m
    flag = is_equal (ps_point%p2 , m**2) .and. is_equal (ps_point%k2 , m**2)
  end function phase_space_point_is_onshell
end module ttv_formfactors
