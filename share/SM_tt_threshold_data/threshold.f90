module @ID@_top_real_decay
  use kinds
  use diagnostics
  use omega95
  use omega_color, OCF => omega_color_factor
  use parameters_SM_tt_threshold
  implicit none
  private
  public :: calculate_amplitude

  integer, parameter :: n_prt = 4
  integer, parameter :: n_flv = 1

  integer, dimension(n_prt,n_flv), save, protected :: table_flavor_states
  data table_flavor_states(:,   1) /   6,  24,   5,  21 / ! t W+ b gl

  type(momentum) :: p1, p2, p3, p4
  type(momentum) :: p12, p14
  type(spinor) :: owf_u3_2__1_0, owf_u3_1__1_0
  type(conjspinor) :: owf_d3b__1_3_0
  type(vector) :: owf_gl___4_0, owf_wm_2_0
  type(spinor) :: owf_d3_1__12_0, owf_u3_1__14_0_X1
  complex(default) :: oks_u3_2_wpd3_1_gl_2_1, oks_u3_1_wpd3_1_gl__

contains

  function calculate_amplitude (k, s, top_width) result (amp)
    complex(default) :: amp
    real(default), dimension(0:3,*), intent(in) :: k
    integer, dimension(n_prt), intent(in) :: s
    real(default), intent(in) :: top_width
    real(default) :: dynamic_top_mass
    p1 = - k(:,1) ! incoming
    p2 =   k(:,2) ! outgoing
    p3 =   k(:,3) ! outgoing
    p4 =   k(:,4) ! outgoing
    p12 = p1 + p2
    p14 = p1 + p4
    dynamic_top_mass = sqrt (p1 * p1)
    owf_u3_1__1_0 = u (dynamic_top_mass, - p1, s(1))
    owf_wm_2_0 = conjg (eps (mass(24), p2, s(2)))
    owf_d3b__1_3_0 = ubar (mass(5), p3, s(3))
    owf_gl___4_0 = conjg (eps (mass(21), p4, s(4)))
    owf_d3_1__12_0 = pr_psi(p12,mass(5),wd_tl(p12,width(5)), .false., &
       + f_vlf(gcc,owf_wm_2_0,owf_u3_1__1_0))
    owf_u3_1__14_0_X1 = pr_psi(p14,dynamic_top_mass,wd_tl(p14,top_width), &
       .false., + f_vf((-gs),owf_gl___4_0,owf_u3_1__1_0))
    oks_u3_1_wpd3_1_gl__ = ( &
       + f_fv((-gs),owf_d3b__1_3_0,owf_gl___4_0))*owf_d3_1__12_0
    oks_u3_1_wpd3_1_gl__ = oks_u3_1_wpd3_1_gl__ + ( &
       + f_fvl(gcc,owf_d3b__1_3_0,owf_wm_2_0))*owf_u3_1__14_0_X1
    amp = - oks_u3_1_wpd3_1_gl__ ! 2 vertices, 1 propagators
  end function calculate_amplitude

end module @ID@_top_real_decay

module @ID@_anti_top_real_decay
  use kinds
  use diagnostics
  use omega95
  use omega_color, OCF => omega_color_factor
  use parameters_SM_tt_threshold
  implicit none
  private
  public :: calculate_amplitude

  integer, parameter :: n_prt = 4
  integer, parameter :: n_flv = 1

  integer, dimension(n_prt,n_flv), save, protected :: table_flavor_states
  data table_flavor_states(:,   1) /  -6, -24,  -5,  21 / ! tbar W- bbar gl

  type(momentum) :: p1, p2, p3, p4
  type(momentum) :: p12, p14
  type(spinor) :: owf_d3_2__3_0
  type(conjspinor) :: owf_u3b__1_1_0
  type(vector) :: owf_gl_1_2_4_0, owf_wp_2_0
  type(conjspinor) :: owf_d3b__1_12_0, owf_u3b__2_14_0
  complex(default) :: oks_u3b__1wmd3b__2gl_2_1

contains

  function calculate_amplitude (k, s, top_width) result (amp)
    complex(default) :: amp
    real(default), dimension(0:3,*), intent(in) :: k
    integer, dimension(n_prt), intent(in) :: s
    real(default), intent(in) :: top_width
    real(default) :: dynamic_top_mass
    p1 = - k(:,1) ! incoming
    p2 =   k(:,2) ! outgoing
    p3 =   k(:,3) ! outgoing
    p4 =   k(:,4) ! outgoing
    p12 = p1 + p2
    p14 = p1 + p4
    dynamic_top_mass = sqrt (p1 * p1)
    owf_u3b__1_1_0 = vbar (dynamic_top_mass, - p1, s(1))
    owf_wp_2_0 = conjg (eps (mass(24), p2, s(2)))
    owf_d3_2__3_0 = v (mass(5), p3, s(3))
    owf_gl_1_2_4_0 = conjg (eps (mass(21), p4, s(4)))
    owf_d3b__1_12_0 = pr_psibar(p12,mass(5),wd_tl(p12,width(5)), .false., &
       + f_fvl(gcc,owf_u3b__1_1_0,owf_wp_2_0))
    owf_u3b__2_14_0 = pr_psibar(p14,dynamic_top_mass,wd_tl(p14,top_width), &
       .false., + f_fv((-gs),owf_u3b__1_1_0,owf_gl_1_2_4_0))
    oks_u3b__1wmd3b__2gl_2_1 = owf_d3b__1_12_0 * &
       f_vf((-gs),owf_gl_1_2_4_0,owf_d3_2__3_0)
    oks_u3b__1wmd3b__2gl_2_1 = oks_u3b__1wmd3b__2gl_2_1 + owf_u3b__2_14_0*( &
       + f_vlf(gcc,owf_wp_2_0,owf_d3_2__3_0))
    amp = - oks_u3b__1wmd3b__2gl_2_1 ! 2 vertices, 1 propagators
  end function calculate_amplitude

end module @ID@_anti_top_real_decay

module @ID@_threshold
  use kinds
  use diagnostics
  use numeric_utils
  use constants
  use lorentz
  use omega95
  use parameters_SM_tt_threshold
  use ttv_formfactors
  use @ID@_top_real_decay, top_real_decay_calculate_amplitude => calculate_amplitude
  use @ID@_anti_top_real_decay, anti_top_real_decay_calculate_amplitude => calculate_amplitude
  use, intrinsic :: iso_fortran_env, only: output_unit
  implicit none
  private
  public :: init, calculate_blob, compute_born, &
       set_production_momenta, init_workspace, compute_production_owfs, &
       compute_decay_owfs, table_spin_states, compute_production_me, &
       top_decay_born, anti_top_decay_born, top_propagators, compute_real, abs2

  ! DON'T EVEN THINK of removing the following!
  ! If the compiler complains about undeclared
  ! or undefined variables, you are compiling
  ! against an incompatible omega95 module!
  integer, dimension(7), parameter, private :: require = &
    (/ omega_spinors_2010_01_A, omega_spinor_cpls_2010_01_A, &
       omega_vectors_2010_01_A, omega_polarizations_2010_01_A, &
       omega_couplings_2010_01_A, omega_color_2010_01_A, &
       omega_utils_2010_01_A /)

  logical, parameter, public :: test_ward = .false.

  integer, parameter, public :: n_prt = 6
  integer, parameter :: n_prt_OS = 4
  integer, parameter, public :: n_in = 2
  integer, parameter, public :: n_out = 4
  integer, parameter, public :: n_cindex = 2
  integer, parameter, public :: n_flv = 1
  integer, parameter, public :: n_hel = 144
  integer, parameter :: n_hel_OS = 16

  type(lorentz_transformation_t) :: boost_to_cms, boost_to_top_rest

  ! NB: you MUST NOT change the value of N_ here!!!
  !     It is defined here for convenience only and must be
  !     compatible with hardcoded values in the amplitude!
  real(default), parameter, public :: N_ = 3
  logical, parameter :: F = .false.
  logical, parameter :: T = .true.
  !!! Colour factors: N_ quarks can be produced
  !!! Helicity factors: Mean over incoming helicities
  real(default), parameter, public :: production_factors = N_ / four

  integer, dimension(2), parameter, public :: ass_quark = [5, 6]
  integer, dimension(2), parameter, public :: ass_boson = [3, 4]

  integer, dimension(n_prt_OS,n_hel_OS), save, protected :: table_spin_states_OS
  data table_spin_states_OS(:,   1) / -1, -1, -1, -1 /
  data table_spin_states_OS(:,   2) / -1, -1, -1,  1 /
  data table_spin_states_OS(:,   3) / -1, -1,  1, -1 /
  data table_spin_states_OS(:,   4) / -1, -1,  1,  1 /
  data table_spin_states_OS(:,   5) / -1,  1, -1, -1 /
  data table_spin_states_OS(:,   6) / -1,  1, -1,  1 /
  data table_spin_states_OS(:,   7) / -1,  1,  1, -1 /
  data table_spin_states_OS(:,   8) / -1,  1,  1,  1 /
  data table_spin_states_OS(:,   9) /  1, -1, -1, -1 /
  data table_spin_states_OS(:,  10) /  1, -1, -1,  1 /
  data table_spin_states_OS(:,  11) /  1, -1,  1, -1 /
  data table_spin_states_OS(:,  12) /  1, -1,  1,  1 /
  data table_spin_states_OS(:,  13) /  1,  1, -1, -1 /
  data table_spin_states_OS(:,  14) /  1,  1, -1,  1 /
  data table_spin_states_OS(:,  15) /  1,  1,  1, -1 /
  data table_spin_states_OS(:,  16) /  1,  1,  1,  1 /

  integer, dimension(n_prt,n_hel), save, protected :: table_spin_states
  data table_spin_states(:,   1) / -1, -1, -1, -1, -1, -1 /
  data table_spin_states(:,   2) / -1, -1, -1, -1, -1,  1 /
  data table_spin_states(:,   3) / -1, -1, -1, -1,  1, -1 /
  data table_spin_states(:,   4) / -1, -1, -1, -1,  1,  1 /
  data table_spin_states(:,   5) / -1, -1, -1,  0, -1, -1 /
  data table_spin_states(:,   6) / -1, -1, -1,  0, -1,  1 /
  data table_spin_states(:,   7) / -1, -1, -1,  0,  1, -1 /
  data table_spin_states(:,   8) / -1, -1, -1,  0,  1,  1 /
  data table_spin_states(:,   9) / -1, -1, -1,  1, -1, -1 /
  data table_spin_states(:,  10) / -1, -1, -1,  1, -1,  1 /
  data table_spin_states(:,  11) / -1, -1, -1,  1,  1, -1 /
  data table_spin_states(:,  12) / -1, -1, -1,  1,  1,  1 /
  data table_spin_states(:,  13) / -1, -1,  0, -1, -1, -1 /
  data table_spin_states(:,  14) / -1, -1,  0, -1, -1,  1 /
  data table_spin_states(:,  15) / -1, -1,  0, -1,  1, -1 /
  data table_spin_states(:,  16) / -1, -1,  0, -1,  1,  1 /
  data table_spin_states(:,  17) / -1, -1,  0,  0, -1, -1 /
  data table_spin_states(:,  18) / -1, -1,  0,  0, -1,  1 /
  data table_spin_states(:,  19) / -1, -1,  0,  0,  1, -1 /
  data table_spin_states(:,  20) / -1, -1,  0,  0,  1,  1 /
  data table_spin_states(:,  21) / -1, -1,  0,  1, -1, -1 /
  data table_spin_states(:,  22) / -1, -1,  0,  1, -1,  1 /
  data table_spin_states(:,  23) / -1, -1,  0,  1,  1, -1 /
  data table_spin_states(:,  24) / -1, -1,  0,  1,  1,  1 /
  data table_spin_states(:,  25) / -1, -1,  1, -1, -1, -1 /
  data table_spin_states(:,  26) / -1, -1,  1, -1, -1,  1 /
  data table_spin_states(:,  27) / -1, -1,  1, -1,  1, -1 /
  data table_spin_states(:,  28) / -1, -1,  1, -1,  1,  1 /
  data table_spin_states(:,  29) / -1, -1,  1,  0, -1, -1 /
  data table_spin_states(:,  30) / -1, -1,  1,  0, -1,  1 /
  data table_spin_states(:,  31) / -1, -1,  1,  0,  1, -1 /
  data table_spin_states(:,  32) / -1, -1,  1,  0,  1,  1 /
  data table_spin_states(:,  33) / -1, -1,  1,  1, -1, -1 /
  data table_spin_states(:,  34) / -1, -1,  1,  1, -1,  1 /
  data table_spin_states(:,  35) / -1, -1,  1,  1,  1, -1 /
  data table_spin_states(:,  36) / -1, -1,  1,  1,  1,  1 /
  data table_spin_states(:,  37) / -1,  1, -1, -1, -1, -1 /
  data table_spin_states(:,  38) / -1,  1, -1, -1, -1,  1 /
  data table_spin_states(:,  39) / -1,  1, -1, -1,  1, -1 /
  data table_spin_states(:,  40) / -1,  1, -1, -1,  1,  1 /
  data table_spin_states(:,  41) / -1,  1, -1,  0, -1, -1 /
  data table_spin_states(:,  42) / -1,  1, -1,  0, -1,  1 /
  data table_spin_states(:,  43) / -1,  1, -1,  0,  1, -1 /
  data table_spin_states(:,  44) / -1,  1, -1,  0,  1,  1 /
  data table_spin_states(:,  45) / -1,  1, -1,  1, -1, -1 /
  data table_spin_states(:,  46) / -1,  1, -1,  1, -1,  1 /
  data table_spin_states(:,  47) / -1,  1, -1,  1,  1, -1 /
  data table_spin_states(:,  48) / -1,  1, -1,  1,  1,  1 /
  data table_spin_states(:,  49) / -1,  1,  0, -1, -1, -1 /
  data table_spin_states(:,  50) / -1,  1,  0, -1, -1,  1 /
  data table_spin_states(:,  51) / -1,  1,  0, -1,  1, -1 /
  data table_spin_states(:,  52) / -1,  1,  0, -1,  1,  1 /
  data table_spin_states(:,  53) / -1,  1,  0,  0, -1, -1 /
  data table_spin_states(:,  54) / -1,  1,  0,  0, -1,  1 /
  data table_spin_states(:,  55) / -1,  1,  0,  0,  1, -1 /
  data table_spin_states(:,  56) / -1,  1,  0,  0,  1,  1 /
  data table_spin_states(:,  57) / -1,  1,  0,  1, -1, -1 /
  data table_spin_states(:,  58) / -1,  1,  0,  1, -1,  1 /
  data table_spin_states(:,  59) / -1,  1,  0,  1,  1, -1 /
  data table_spin_states(:,  60) / -1,  1,  0,  1,  1,  1 /
  data table_spin_states(:,  61) / -1,  1,  1, -1, -1, -1 /
  data table_spin_states(:,  62) / -1,  1,  1, -1, -1,  1 /
  data table_spin_states(:,  63) / -1,  1,  1, -1,  1, -1 /
  data table_spin_states(:,  64) / -1,  1,  1, -1,  1,  1 /
  data table_spin_states(:,  65) / -1,  1,  1,  0, -1, -1 /
  data table_spin_states(:,  66) / -1,  1,  1,  0, -1,  1 /
  data table_spin_states(:,  67) / -1,  1,  1,  0,  1, -1 /
  data table_spin_states(:,  68) / -1,  1,  1,  0,  1,  1 /
  data table_spin_states(:,  69) / -1,  1,  1,  1, -1, -1 /
  data table_spin_states(:,  70) / -1,  1,  1,  1, -1,  1 /
  data table_spin_states(:,  71) / -1,  1,  1,  1,  1, -1 /
  data table_spin_states(:,  72) / -1,  1,  1,  1,  1,  1 /
  data table_spin_states(:,  73) /  1, -1, -1, -1, -1, -1 /
  data table_spin_states(:,  74) /  1, -1, -1, -1, -1,  1 /
  data table_spin_states(:,  75) /  1, -1, -1, -1,  1, -1 /
  data table_spin_states(:,  76) /  1, -1, -1, -1,  1,  1 /
  data table_spin_states(:,  77) /  1, -1, -1,  0, -1, -1 /
  data table_spin_states(:,  78) /  1, -1, -1,  0, -1,  1 /
  data table_spin_states(:,  79) /  1, -1, -1,  0,  1, -1 /
  data table_spin_states(:,  80) /  1, -1, -1,  0,  1,  1 /
  data table_spin_states(:,  81) /  1, -1, -1,  1, -1, -1 /
  data table_spin_states(:,  82) /  1, -1, -1,  1, -1,  1 /
  data table_spin_states(:,  83) /  1, -1, -1,  1,  1, -1 /
  data table_spin_states(:,  84) /  1, -1, -1,  1,  1,  1 /
  data table_spin_states(:,  85) /  1, -1,  0, -1, -1, -1 /
  data table_spin_states(:,  86) /  1, -1,  0, -1, -1,  1 /
  data table_spin_states(:,  87) /  1, -1,  0, -1,  1, -1 /
  data table_spin_states(:,  88) /  1, -1,  0, -1,  1,  1 /
  data table_spin_states(:,  89) /  1, -1,  0,  0, -1, -1 /
  data table_spin_states(:,  90) /  1, -1,  0,  0, -1,  1 /
  data table_spin_states(:,  91) /  1, -1,  0,  0,  1, -1 /
  data table_spin_states(:,  92) /  1, -1,  0,  0,  1,  1 /
  data table_spin_states(:,  93) /  1, -1,  0,  1, -1, -1 /
  data table_spin_states(:,  94) /  1, -1,  0,  1, -1,  1 /
  data table_spin_states(:,  95) /  1, -1,  0,  1,  1, -1 /
  data table_spin_states(:,  96) /  1, -1,  0,  1,  1,  1 /
  data table_spin_states(:,  97) /  1, -1,  1, -1, -1, -1 /
  data table_spin_states(:,  98) /  1, -1,  1, -1, -1,  1 /
  data table_spin_states(:,  99) /  1, -1,  1, -1,  1, -1 /
  data table_spin_states(:, 100) /  1, -1,  1, -1,  1,  1 /
  data table_spin_states(:, 101) /  1, -1,  1,  0, -1, -1 /
  data table_spin_states(:, 102) /  1, -1,  1,  0, -1,  1 /
  data table_spin_states(:, 103) /  1, -1,  1,  0,  1, -1 /
  data table_spin_states(:, 104) /  1, -1,  1,  0,  1,  1 /
  data table_spin_states(:, 105) /  1, -1,  1,  1, -1, -1 /
  data table_spin_states(:, 106) /  1, -1,  1,  1, -1,  1 /
  data table_spin_states(:, 107) /  1, -1,  1,  1,  1, -1 /
  data table_spin_states(:, 108) /  1, -1,  1,  1,  1,  1 /
  data table_spin_states(:, 109) /  1,  1, -1, -1, -1, -1 /
  data table_spin_states(:, 110) /  1,  1, -1, -1, -1,  1 /
  data table_spin_states(:, 111) /  1,  1, -1, -1,  1, -1 /
  data table_spin_states(:, 112) /  1,  1, -1, -1,  1,  1 /
  data table_spin_states(:, 113) /  1,  1, -1,  0, -1, -1 /
  data table_spin_states(:, 114) /  1,  1, -1,  0, -1,  1 /
  data table_spin_states(:, 115) /  1,  1, -1,  0,  1, -1 /
  data table_spin_states(:, 116) /  1,  1, -1,  0,  1,  1 /
  data table_spin_states(:, 117) /  1,  1, -1,  1, -1, -1 /
  data table_spin_states(:, 118) /  1,  1, -1,  1, -1,  1 /
  data table_spin_states(:, 119) /  1,  1, -1,  1,  1, -1 /
  data table_spin_states(:, 120) /  1,  1, -1,  1,  1,  1 /
  data table_spin_states(:, 121) /  1,  1,  0, -1, -1, -1 /
  data table_spin_states(:, 122) /  1,  1,  0, -1, -1,  1 /
  data table_spin_states(:, 123) /  1,  1,  0, -1,  1, -1 /
  data table_spin_states(:, 124) /  1,  1,  0, -1,  1,  1 /
  data table_spin_states(:, 125) /  1,  1,  0,  0, -1, -1 /
  data table_spin_states(:, 126) /  1,  1,  0,  0, -1,  1 /
  data table_spin_states(:, 127) /  1,  1,  0,  0,  1, -1 /
  data table_spin_states(:, 128) /  1,  1,  0,  0,  1,  1 /
  data table_spin_states(:, 129) /  1,  1,  0,  1, -1, -1 /
  data table_spin_states(:, 130) /  1,  1,  0,  1, -1,  1 /
  data table_spin_states(:, 131) /  1,  1,  0,  1,  1, -1 /
  data table_spin_states(:, 132) /  1,  1,  0,  1,  1,  1 /
  data table_spin_states(:, 133) /  1,  1,  1, -1, -1, -1 /
  data table_spin_states(:, 134) /  1,  1,  1, -1, -1,  1 /
  data table_spin_states(:, 135) /  1,  1,  1, -1,  1, -1 /
  data table_spin_states(:, 136) /  1,  1,  1, -1,  1,  1 /
  data table_spin_states(:, 137) /  1,  1,  1,  0, -1, -1 /
  data table_spin_states(:, 138) /  1,  1,  1,  0, -1,  1 /
  data table_spin_states(:, 139) /  1,  1,  1,  0,  1, -1 /
  data table_spin_states(:, 140) /  1,  1,  1,  0,  1,  1 /
  data table_spin_states(:, 141) /  1,  1,  1,  1, -1, -1 /
  data table_spin_states(:, 142) /  1,  1,  1,  1, -1,  1 /
  data table_spin_states(:, 143) /  1,  1,  1,  1,  1, -1 /
  data table_spin_states(:, 144) /  1,  1,  1,  1,  1,  1 /

  complex(default), dimension(:), allocatable, save, public :: amp_blob
  complex(default), dimension(:), allocatable, save, public :: amp_tree
  integer, public :: nhel_max

  type(momentum) :: p1, p2, p3, p4, p5, p6
  type(momentum) :: p12, p35, p46
  type(momentum) :: mom_top_onshell, mom_top_onshell_rest
  type(momentum) :: mom_topbar_onshell, mom_topbar_onshell_rest
  type(momentum) :: mom_wm_onshell, mom_wm_onshell_rest
  type(momentum) :: mom_wp_onshell, mom_wp_onshell_rest
  type(momentum) :: mom_b_onshell, mom_b_onshell_rest
  type(momentum) :: mom_bbar_onshell, mom_bbar_onshell_rest
  type(spinor) :: owf_t_4, owf_b_6, owf_e_1
  type(conjspinor) :: owf_t_3, owf_b_5, owf_e_2
  type(vector) :: owf_Wp_3, owf_Wm_4
  type(spinor) :: owf_wb_46
  type(conjspinor) :: owf_wb_35
  type(vector) :: owf_A_12, owf_Z_12

contains

  subroutine init (par, scheme)
    real(default), dimension(*), intent(in) :: par
    integer, intent(in) :: scheme
    call import_from_whizard (par, scheme)
  end subroutine init

  subroutine compute_production_owfs (hi, spins)
    integer, intent(in), optional :: hi
    integer, dimension(2), intent(in), optional :: spins
    integer, dimension(n_prt_OS) :: s_OS
    integer, dimension(2) :: s
    if (onshell_tops (p3, p4)) then
       s_OS = table_spin_states_OS(:,hi)
       owf_e_1 = u (mass(11), - p1, s_OS(1))
       owf_e_2 = vbar (mass(11), - p2, s_OS(2))
       owf_t_3 = ubar (ttv_mtpole(p12*p12), p3, s_OS(3))
       owf_t_4 = v (ttv_mtpole(p12*p12), p4, s_OS(4))
       owf_A_12 = pr_feynman (p12, v_ff (qlep, owf_e_2, owf_e_1))
       owf_Z_12 = pr_unitarity (p12, mass(23), wd_tl (p12, width(23)), &
            .false., + va_ff (gnclep(1), gnclep(2), owf_e_2, owf_e_1))
    else
       if (present (hi)) then
          s = table_spin_states(1:2,hi)
       else
          if (present (spins)) then
             s = spins
          else
             call msg_fatal ("compute_production_owfs: " // &
                  "Please give either helicity index or spins")
          end if
       end if
       owf_e_1 = u (mass(11), - p1, s(1))
       owf_e_2 = vbar (mass(11), - p2, s(2))
       owf_A_12 = pr_feynman (p12, v_ff (qlep, owf_e_2, owf_e_1))
       owf_Z_12 = pr_unitarity (p12, mass(23), wd_tl (p12, width(23)), &
            .false., + va_ff (gnclep(1), gnclep(2), owf_e_2, owf_e_1))
    end if
  end subroutine compute_production_owfs

  subroutine compute_decay_owfs (hi, spins)
    integer, intent(in), optional :: hi
    integer, dimension(3:6), intent(in), optional :: spins
    integer, dimension(3:6) :: s
    if (present (hi)) then
       s = table_spin_states(3:6,hi)
    else
       if (present (spins)) then
          s = spins
       else
          call msg_fatal ("compute_decay_owfs: " // &
               "Please give either helicity index or spins")
       end if
    end if
    owf_Wp_3 = conjg (eps (mass(24), p3, s(THR_POS_WP)))
    owf_Wm_4 = conjg (eps (mass(24), p4, s(THR_POS_WM)))
    owf_b_5 = ubar (mass(5), p5, s(THR_POS_B))
    owf_b_6 = v (mass(5), p6, s(THR_POS_BBAR))
  end subroutine compute_decay_owfs

  function calculate_blob (ffi, h_t, h_tbar) result (amp)
    complex(default) :: amp
    integer, intent(in) :: ffi
    integer, intent(in), optional :: h_t, h_tbar
    complex(default) :: blob_Z_vec, blob_Z_ax, ttv_vec, ttv_ax
    real(default) :: mtop, top_width
    type(momentum) :: ptop, ptopbar
    integer :: u
    u = output_unit
    if (onshell_tops (p3, p4)) then
       blob_Z_vec = gncup(1) * ttv_formfactor (p3, p4, 1)
       blob_Z_ax = gncup(2) * ttv_formfactor (p3, p4, 2)
       amp = owf_Z_12 * va_ff (blob_Z_vec, blob_Z_ax, owf_t_3, owf_t_4)
       amp = amp + owf_A_12 * v_ff (qup, owf_t_3, owf_t_4) * &
            ttv_formfactor (p3, p4, 1)
    else
       ttv_vec = ttv_formfactor (p35, p46, 1, ffi)
       ttv_ax = ttv_formfactor (p35, p46, 2, ffi)
       blob_Z_vec = gncup(1) * ttv_vec
       blob_Z_ax = gncup(2) * ttv_ax
       mtop = ttv_mtpole (p12*p12)
       if (threshold%settings%onshell_projection%production) then
          if (debug_active (D_THRESHOLD)) then
             call assert_equal (u, sqrt (mom_top_onshell * mom_top_onshell), &
                  mtop, "ptop is projected", exit_on_fail=.true.)
             call assert_equal (u, sqrt (mom_topbar_onshell * mom_topbar_onshell), &
                  mtop, "ptop is projected", exit_on_fail=.true.)
          end if
          ptop = mom_top_onshell
          ptopbar = mom_topbar_onshell
       else
          ptop = p35
          ptopbar = p46
       end if
       if (threshold%settings%factorized_computation) then
          owf_t_3 = ubar (sqrt (ptop * ptop), ptop, h_t)
          owf_t_4 = v (sqrt (ptopbar * ptopbar), ptopbar, h_tbar)
          amp = owf_Z_12 * va_ff (blob_Z_vec, blob_Z_ax, owf_t_3, owf_t_4)
          amp = amp + owf_A_12 * v_ff (qup, owf_t_3, owf_t_4) * ttv_vec
          amp = - amp
       else
          top_width = ttv_wtpole (p12*p12, ffi)
          owf_wb_35 = pr_psibar (ptop, mtop, wd_tl (ptop, top_width), .false., &
               + f_fvl (gccq33, owf_b_5, owf_Wp_3))
          owf_wb_46 = pr_psi (ptopbar, mtop, wd_tl (ptopbar, top_width), .false., &
               + f_vlf (gccq33, owf_Wm_4, owf_b_6))
          amp = owf_Z_12 * va_ff (blob_Z_vec, blob_Z_ax, owf_wb_35, owf_wb_46)
          amp = amp + owf_A_12 * v_ff (qup, owf_wb_35, owf_wb_46) * ttv_vec
       end if
    end if
  end function calculate_blob

  function top_propagators (ffi) result(one_over_p)
    complex(default) :: one_over_p
    integer, intent(in) :: ffi
    real(default) :: top_mass, top_width
    top_mass = ttv_mtpole (p12*p12)
    if (threshold%settings%onshell_projection%width) then
      top_width = ttv_wtpole (p12*p12, ffi)
      one_over_p = one / cmplx (p35*p35 - top_mass**2, top_mass*top_width, kind=default)
      one_over_p = one_over_p / cmplx (p46*p46 - top_mass**2, &
           top_mass * top_width, kind=default)
    else
      call msg_fatal ("on-shell projection width: You should really keep it on-shell")
      top_width = ttv_wtpole (sqrt(p35*p35), ffi, use_as_minv=.true.)
      one_over_p = one / cmplx (p35*p35 - top_mass**2, top_mass*top_width, kind=default)
      top_width = ttv_wtpole (sqrt(p46*p46), ffi, use_as_minv=.true.)
      one_over_p = one_over_p / cmplx (p46*p46 - top_mass**2, &
           top_mass*top_width, kind=default)
    end if
  end function top_propagators

  function top_decay_born (h_t, h_W, h_b) result (me)
    complex(default) :: me
    integer, intent(in) :: h_t
    integer, intent(in) :: h_W, h_b
    type(momentum) :: pw, pb, ptop
    call set_top_decay_momenta (pw, pb, ptop)
    owf_Wp_3 = conjg (eps (mass(24), pw, h_W))
    owf_b_5 = ubar (mass(5), pb, h_b)
    me = f_fvl (gccq33, owf_b_5, owf_Wp_3) * u (sqrt(ptop*ptop), ptop, h_t)
  end function top_decay_born

  subroutine set_top_decay_momenta (pwp, pb, ptop)
    type(momentum), intent(out) :: pwp, pb, ptop
    if (threshold%settings%onshell_projection%decay) then
       if (threshold%settings%onshell_projection%boost_decay) then
         pwp = mom_wp_onshell
         pb = mom_b_onshell
         ptop = mom_top_onshell
       else
         pwp = mom_wp_onshell_rest
         pb = mom_b_onshell_rest
         ptop = mom_top_onshell_rest
       end if
       if (debug_active (D_THRESHOLD)) then
          call assert_equal (output_unit, sqrt (ptop * ptop), mass(6), &
            "ptop is projected", rel_smallness=tiny_07, exit_on_fail=.true.)
          call assert_equal (output_unit, sqrt (pwp * pwp), mass(24), &
            "pwp is projected", rel_smallness=tiny_07, exit_on_fail=.true.)
          call assert_equal (output_unit, sqrt (pb * pb), mass(5), &
            "pb is projected", rel_smallness=tiny_07, exit_on_fail=.true.)
       end if
    else
       pwp = p3
       pb = p5
       ptop = p35
    end if
  end subroutine set_top_decay_momenta

  function anti_top_decay_born (h_tbar, h_W, h_b) result (me)
    complex(default) :: me
    integer, intent(in) :: h_tbar
    integer, intent(in) :: h_W, h_b
    type(momentum) :: pwm, pbbar, ptopbar
    call set_anti_top_decay_momenta (pwm, pbbar, ptopbar)
    owf_Wm_4 = conjg (eps (mass(24), pwm, h_W))
    owf_b_6 = v (mass(5), pbbar, h_b)
    me = vbar (sqrt(ptopbar*ptopbar), ptopbar, h_tbar) * &
         f_vlf (gccq33, owf_Wm_4, owf_b_6)
  end function anti_top_decay_born

  subroutine set_anti_top_decay_momenta (pwm, pbbar, ptopbar)
    type(momentum), intent(out) :: pwm, pbbar, ptopbar
    if (threshold%settings%onshell_projection%decay) then
       if (threshold%settings%onshell_projection%boost_decay) then
          pwm = mom_wm_onshell
          pbbar = mom_bbar_onshell
          ptopbar = mom_topbar_onshell
       else
          pwm = mom_wm_onshell_rest
          pbbar = mom_bbar_onshell_rest
          ptopbar = mom_topbar_onshell_rest
       end if
       if (debug_active (D_THRESHOLD)) then
          call assert_equal (output_unit, sqrt (ptopbar * ptopbar), mass(6), &
               "ptopbar is projected", rel_smallness=tiny_07, exit_on_fail=.true.)
          call assert_equal (output_unit, sqrt (pwm * pwm), mass(24), &
            "pwm is projected", rel_smallness=tiny_07, exit_on_fail=.true.)
          call assert_equal (output_unit, sqrt (pbbar * pbbar), mass(5), &
            "pbbar is projected", rel_smallness=tiny_07, exit_on_fail=.true.)
       end if
    else
       pwm = p4
       pbbar = p6
       ptopbar = p46
    end if
  end subroutine set_anti_top_decay_momenta

  subroutine compute_born (k, ffi)
    real(default), dimension(0:3,*), intent(in) :: k
    integer, intent(in) :: ffi
    complex(default), dimension(-1:1,-1:1,-1:1,-1:1) :: production_me
    complex(default), dimension(-1:1,-1:1,-1:1,1:2) :: born_decay_me
    complex(default) :: prod, dec1, dec2
    integer, dimension(n_prt) :: s
    integer :: hi, h_t, h_tbar
    call set_production_momenta (k)
    call init_workspace ()
    if (threshold%settings%factorized_computation) then
       production_me = compute_production_me (ffi)
       born_decay_me = compute_decay_me ()
    end if
    do hi = 1, nhel_max
       s = table_spin_states(:,hi)
       ! TODO: (bcn 2016-02-08) in the matched factorized computation we might
       !        need interference terms in the Born
       if (threshold%settings%factorized_computation) then
          if (threshold%settings%helicity_approximated) then
             if (threshold%settings%helicity_approximated_extra) then
                prod = zero
                do h_t = -1, 1, 2
                   do h_tbar = -1, 1, 2
                      prod = prod + abs2(production_me(s(1), s(2), h_t, h_tbar))
                   end do
                end do
                dec1 = zero
                do h_t = -1, 1, 2
                   dec1 = dec1 + abs2(born_decay_me(s(ass_quark(1)), s(ass_boson(1)), h_t, 1))
                end do
                !print *, 'dec1 =    ', dec1 !!! Debugging
                !stop
                dec2 = zero
                do h_tbar = -1, 1, 2
                   dec2 = dec2 + abs2(born_decay_me(s(ass_quark(2)), s(ass_boson(2)), h_tbar, 2))
                end do
                amp_blob(hi) = amp_blob(hi) + &
                     prod * abs2 (top_propagators (ffi)) * &
                     dec1 * dec2 / 4
             else
                do h_t = -1, 1, 2
                   do h_tbar = -1, 1, 2
                      prod = production_me(s(1), s(2), h_t, h_tbar)
                      dec1 = born_decay_me(s(ass_quark(1)), s(ass_boson(1)), h_t, 1)
                      dec2 = born_decay_me(s(ass_quark(2)), s(ass_boson(2)), h_tbar, 2)
                      amp_blob(hi) = amp_blob(hi) + &
                           abs2 (prod) * abs2 (top_propagators (ffi)) * &
                           abs2 (dec1) * abs2 (dec2)
                   end do
                end do
             end if
          else
             do h_t = -1, 1, 2
                do h_tbar = -1, 1, 2
                   prod = production_me(s(1), s(2), h_t, h_tbar)
                   dec1 = born_decay_me(s(ass_quark(1)), s(ass_boson(1)), h_t, 1)
                   dec2 = born_decay_me(s(ass_quark(2)), s(ass_boson(2)), h_tbar, 2)
                   amp_blob(hi) = amp_blob(hi) + &
                        prod * top_propagators (ffi) * &
                        dec1 * dec2
                end do
             end do
          end if
       else
          call compute_production_owfs (hi)
          if (.not. onshell_tops (p3, p4))  call compute_decay_owfs (hi)
          amp_blob(hi) = - calculate_blob (ffi) ! 4 vertices, 3 propagators
       end if
    end do
  end subroutine compute_born

  function compute_decay_me () result (born_decay_me)
    complex(default), dimension(-1:1,-1:1,-1:1,1:2) :: born_decay_me
    procedure(top_decay_born), pointer :: top_decay_born_
    integer :: h_t, h_W, h_b, leg
    do leg = 1, 2
       if (leg == 1) then
          top_decay_born_ => anti_top_decay_born
       else
          top_decay_born_ => top_decay_born
       end if
       do h_t = -1, 1, 2
          do h_W = -1, 1, 1
             do h_b = -1, 1, 2
                born_decay_me(h_b, h_W, h_t, leg) = top_decay_born_ (h_t, h_W, h_b)
             end do
          end do
       end do
    end do
  end function compute_decay_me

  subroutine init_workspace ()
    if (onshell_tops (p3, p4)) then
       nhel_max = n_hel_OS
    else
       nhel_max = n_hel
    end if
    if (allocated (amp_blob))  amp_blob = zero
  end subroutine init_workspace

  subroutine set_production_momenta (k)
    real(default), dimension(0:3,*), intent(in) :: k
    if (debug2_active (D_THRESHOLD)) then
       call msg_debug (D_THRESHOLD, "set_production_momenta")
       print *, 'k =    ', k(0:3,1:6)
    end if
    p1 = - k(:,1) ! incoming
    p2 = - k(:,2) ! incoming
    p3 =   k(:,3) ! outgoing
    p4 =   k(:,4) ! outgoing
    p12 = p1 + p2
    if (.not. onshell_tops (p3, p4)) then
       p5 =   k(:,5) ! outgoing
       p6 =   k(:,6) ! outgoing
       p35 = p3 + p5
       p46 = p4 + p6
       if (threshold%settings%onshell_projection%active ()) then
          call compute_projected_top_momenta (p12)
          call compute_projected_top_decay_products (p12)
       end if
    end if
  end subroutine set_production_momenta

  subroutine compute_projected_top_momenta (p12)
    type(momentum), intent(in) :: p12
    real(default) :: sqrts, scale_factor, mtop
    real(default), dimension(1:3) :: unit_vec
    real(default), dimension(4) :: tmp, test
    type(vector4_t) :: v4_top_onshell, tmp_v4
    type(vector4_t) :: v4_topbar_onshell, v4_topbar_onshell_rest
    integer :: u
    u = output_unit
    mtop = ttv_mtpole (p12*p12)
    sqrts = - p12%t
    scale_factor = sqrt (sqrts**2 - 4 * mtop**2) / 2
    unit_vec = p35%x / sqrt (dot_product(p35%x, p35%x))
    mom_top_onshell = [sqrts / 2, scale_factor * unit_vec]
    tmp = mom_top_onshell
    v4_top_onshell = tmp
    mom_top_onshell_rest = [mtop, zero, zero, zero]
    mom_topbar_onshell_rest = [mtop, zero, zero, zero]
    boost_to_cms = boost (v4_top_onshell, mtop)
    boost_to_top_rest = inverse (boost_to_cms)
    mom_topbar_onshell = [sqrts / 2, - scale_factor * unit_vec]
    if (debug_active (D_THRESHOLD)) then
       tmp_v4 = boost_to_top_rest * v4_top_onshell
       tmp = tmp_v4
       test = mom_top_onshell_rest
       call assert_equal (u, tmp, test, &
            "verify that we have the right boost", abs_smallness=tiny_07 * 10, &
             rel_smallness=tiny_07, &
            exit_on_fail=.true.)
       tmp = apply_boost (boost_to_cms, mom_top_onshell_rest)
       test = mom_top_onshell
       call assert_equal(u, test, tmp, "test the inverse boost", &
            exit_on_fail=.true.)
       call assert (u, p12 == - (mom_top_onshell + mom_topbar_onshell), &
            "momentum conservation with a flip", exit_on_fail=.true.)
       call assert_equal (u, mom_top_onshell * mom_top_onshell, mtop**2, &
            "mass onshell", abs_smallness=tiny_07, &
             rel_smallness=tiny_07, exit_on_fail=.true.)
       call assert_equal (u, mom_topbar_onshell * mom_topbar_onshell, mtop**2, &
            "mass onshell", abs_smallness=tiny_07, &
             rel_smallness=tiny_07, exit_on_fail=.true.)
       call assert_equal (u, dot_product(unit_vec, unit_vec), one, &
            "unit vector length", exit_on_fail=.true.)
    end if
  end subroutine compute_projected_top_momenta

  subroutine compute_projected_top_decay_products (p12)
    type(momentum), intent(in) :: p12
    real(default) :: sqrts, scale_factor, mtop, mw2, mb2, en_w, en_b, p_three_mag
    real(default), dimension(1:3) :: unit_vec_wp, unit_vec_wm
    real(default), dimension(4) :: tmp, test
    integer :: u
    u = output_unit
    mtop = ttv_mtpole (p12*p12)
    mw2 = mass(24)**2
    mb2 = mass(5)**2
    en_w = (mtop**2 + mw2 - mb2) / (2 * mtop)
    en_b = (mtop**2 - mw2 + mb2) / (2 * mtop)
    p_three_mag = sqrt (lambda (mtop**2, mw2, mb2)) / (2 * mtop)
    unit_vec_wp = p3%x / sqrt (dot_product(p3%x, p3%x))
    mom_wp_onshell_rest = [en_w, p_three_mag * unit_vec_wp]
    mom_b_onshell_rest = [en_b, - p_three_mag * unit_vec_wp]
    unit_vec_wm = p4%x / sqrt (dot_product(p4%x, p4%x))
    mom_wm_onshell_rest = [en_w, p_three_mag * unit_vec_wm]
    mom_bbar_onshell_rest = [en_b, - p_three_mag * unit_vec_wm]
    mom_wp_onshell = apply_boost (boost_to_cms, mom_wp_onshell_rest)
    mom_b_onshell = apply_boost (boost_to_cms, mom_b_onshell_rest)
    mom_wm_onshell = apply_boost (boost_to_top_rest, mom_wm_onshell_rest)
    mom_bbar_onshell = apply_boost (boost_to_top_rest, mom_bbar_onshell_rest)
    if (debug_active (D_THRESHOLD)) then
       call assert_equal (u, en_w + en_b, mtop, "top energy", &
            exit_on_fail=.true.)
       tmp = mom_wp_onshell + mom_b_onshell
       test = mom_top_onshell
       call assert_equal (u, tmp, test, "overall: top momentum conservation", &
            exit_on_fail=.true.)
       tmp = mom_wm_onshell + mom_bbar_onshell
       test = mom_topbar_onshell
       call assert_equal (u, tmp, test, "overall: topbar momentum conservation", &
            exit_on_fail=.true.)
       tmp = mom_wp_onshell + mom_b_onshell + mom_wm_onshell + mom_bbar_onshell
       test = - p12
       call assert_equal (u, tmp, test, "overall: momentum conservation", &
            abs_smallness=tiny_07, &
            rel_smallness=tiny_07, &
            exit_on_fail=.true.)
       call assert (u, (mom_wp_onshell_rest + mom_b_onshell_rest) == &
            mom_top_onshell_rest, &
            "top: momentum conservation", exit_on_fail=.true.)
       call assert (u, (mom_wm_onshell_rest + mom_bbar_onshell_rest) == &
            mom_top_onshell_rest, &
            "topbar: momentum conservation", exit_on_fail=.true.)
       call assert_equal (u, mom_wp_onshell * mom_wp_onshell, mw2, &
            "mass onshell", rel_smallness=tiny_07, &
            exit_on_fail=.true.)
       call assert_equal (u, mom_wm_onshell * mom_wm_onshell, mw2, &
            "mass onshell", rel_smallness=tiny_07, &
            exit_on_fail=.true.)
       call assert_equal (u, mom_b_onshell * mom_b_onshell, mb2, &
            "mass onshell", rel_smallness=tiny_07, &
            exit_on_fail=.true.)
       call assert_equal (u, mom_bbar_onshell * mom_bbar_onshell, mb2, &
            "mass onshell", rel_smallness=tiny_07, &
            exit_on_fail=.true.)
       call assert_equal (u, dot_product(unit_vec_wp, unit_vec_wp), &
            one, "unit vector length", exit_on_fail=.true.)
       call assert_equal (u, dot_product(unit_vec_wm, unit_vec_wm), &
            one, "unit vector length", exit_on_fail=.true.)
    end if
  end subroutine compute_projected_top_decay_products

  pure function apply_boost (boost_in, mom) result (mom_result)
    type(momentum), intent(in) :: mom
    type(lorentz_transformation_t), intent(in) :: boost_in
    type(momentum) :: mom_result
    type(vector4_t) :: tmp_v4
    real(default), dimension(4) :: tmp
    tmp = mom
    tmp_v4 = tmp
    tmp = boost_in * tmp_v4
    mom_result = tmp
  end function apply_boost

  function compute_production_me (ffi) result (production_me)
    complex(default), dimension(-1:1,-1:1,-1:1,-1:1) :: production_me
    integer, intent(in) :: ffi
    integer :: h_t, h_tbar, h_pos, h_el
    do h_tbar = -1, 1, 2
    do h_t = -1, 1, 2
    do h_pos = -1, 1, 2
    do h_el = -1, 1, 2
       call compute_production_owfs (spins = [h_el, h_pos])
       production_me(h_el, h_pos, h_t, h_tbar) = calculate_blob (ffi, h_t, h_tbar)
    end do
    end do
    end do
    end do
  end function compute_production_me

  function compute_real (k, ffi) result (amp2)
    real(default) :: amp2
    real(default), dimension(0:3,*), intent(in) :: k
    integer, intent(in) :: ffi
    real(default), dimension(0:3,6) :: k_production
    real(default), dimension(0:3,4) :: k_decay_real
    real(default), dimension(0:3,3) :: k_decay_born
    complex(default), dimension(-1:1,-1:1,-1:1,-1:1,1:2) :: production_me
    complex(default), dimension(-1:1,-1:1,-1:1,-1:1,1:2) :: real_decay_me
    complex(default), dimension(-1:1,-1:1,-1:1,1:2) :: born_decay_me
    complex(default), dimension(1:2) :: top_propagators_
    complex(default) :: real_, born_, prod_
    real(default) :: total
    integer, dimension(2) :: h_ass_t
    integer, dimension(n_prt) :: s
    integer :: i, hi, leg, other_leg, h_t, h_tbar, h_gl, h_W, h_b
    if (.not. threshold%settings%factorized_computation)  call msg_fatal ('compute_real: OFFSHELL_STRATEGY is not factorized')
    call init_decay_and_production_momenta ()
    call init_workspace ()
    call compute_amplitudes ()
    total = zero
    do hi = 1, nhel_max
       s = table_spin_states(:,hi)
       do h_t = -1, 1, 2
       do h_tbar = -1, 1, 2
          h_ass_t = [h_t, h_tbar]
          do leg = 1, 2
             other_leg = 3 - leg
             prod_ = production_me(s(1), s(2), h_t, h_tbar, leg)
             born_ = born_decay_me(s(ass_quark(other_leg)), &
                  s(ass_boson(other_leg)), h_ass_t(other_leg), other_leg)
             do h_gl = -1, 1, 2
                real_ = real_decay_me(h_gl, s(ass_quark(leg)), &
                     s(ass_boson(leg)), h_ass_t(leg), leg)
                total = total + abs2 (prod_) * abs2 (real_) * abs2 (born_) * &
                     abs2(top_propagators_ (leg))
             end do
          end do
       end do
       end do
    end do
    amp2 = total * (N_**2 - one) / N_

  contains

    subroutine compute_amplitudes ()
      procedure(top_real_decay_calculate_amplitude), pointer :: top_decay_real
      procedure(top_decay_born), pointer :: top_decay_born_
      do leg = 1, 2
         other_leg = 3 - leg
         call set_decay_and_production_momenta ()
         production_me(:,:,:,:,leg) = compute_production_me (ffi)
         if (leg == 1) then
            top_decay_real => top_real_decay_calculate_amplitude
            top_decay_born_ => anti_top_decay_born
         else
            top_decay_real => anti_top_real_decay_calculate_amplitude
            top_decay_born_ => top_decay_born
         end if
         do h_t = -1, 1, 2
         do h_W = -1, 1, 1
         do h_b = -1, 1, 2
            born_decay_me(h_b, h_W, h_t, leg) = top_decay_born_ (h_t, h_W, h_b)
            if (.not. test_ward) then
               do h_gl = -1, 1, 2
                  real_decay_me(h_gl, h_b, h_W, h_t, leg) = top_decay_real &
                       (k_decay_real, [h_t, h_W, h_b, h_gl], zero)
               end do
            else
               do h_gl = -1, 1, 2
                  real_decay_me(h_gl, h_b, h_W, h_t, leg) = top_decay_real &
                       (k_decay_real, [h_t, h_W, h_b, 4], zero)
               end do
            end if
         end do
         end do
         end do
         top_propagators_(leg) = top_propagators (ffi)
      end do
    end subroutine compute_amplitudes

    subroutine init_decay_and_production_momenta ()
      do i = 1, 6
         k_production(:,i) = k(:,i)
      end do
    end subroutine init_decay_and_production_momenta

    subroutine set_decay_and_production_momenta ()
      k_production(:,ass_quark(other_leg)) = k(:,ass_quark(other_leg))
      k_production(:,ass_quark(leg)) = k(:,ass_quark(leg)) + k(:,7)
      k_decay_real = zero
      k_decay_real(:,4) = k(:,7)
      k_decay_real(:,3) = k(:,ass_quark(leg))
      k_decay_real(:,2) = k(:,ass_boson(leg))
      k_decay_real(:,1) = sum(k_decay_real,2)     !!! momentum conservation
      k_decay_born = zero
      k_decay_born(:,2) = k(:,ass_boson(other_leg))
      k_decay_born(:,3) = k(:,ass_quark(other_leg))
      k_decay_born(:,1) = sum(k_decay_born,2)     !!! momentum conservation
      call set_production_momenta (k_production)
    end subroutine set_decay_and_production_momenta

  end function compute_real

end module @ID@_threshold

subroutine @ID@_threshold_init (par, scheme) bind(C)
  use iso_c_binding
  use kinds
  use @ID@_threshold
  implicit none
  real(c_default_float), dimension(*), intent(in) :: par
  integer(c_int), intent(in) :: scheme
  call init (par, scheme)
end subroutine @ID@_threshold_init

subroutine @ID@_get_amp_squared (amp2, p) bind(C)
  use iso_c_binding
  use kinds
  use constants
  use numeric_utils
  use opr_@ID@, full_proc_new_event => new_event
  use opr_@ID@, full_proc_get_amplitude => get_amplitude
  use opr_@ID@, full_proc_number_spin_states => number_spin_states
  use opr_@ID@, full_proc_number_particles_out => number_particles_out
  use @ID@_threshold
  use parameters_sm_tt_threshold
  use ttv_formfactors
  implicit none
  real(c_default_float), intent(out) :: amp2
  real(c_default_float), dimension(0:3,*), intent(in) :: p
  complex(default), dimension(:), allocatable, save :: amp_summed
  logical :: real_computation
  integer :: i, hi, n_total_hel
  real_computation = full_proc_number_particles_out () == 5
  i = full_proc_number_particles_out () + 2
  if (real_computation) then
     if (.not. allocated (amp_tree)) then
        n_total_hel = n_hel * 2 ! times 2 helicities due to the gluon
        call allocate_amps ()
     end if
     amp_tree = zero
     amp_summed = zero
     call threshold%formfactor%activate ()
     amp2 = compute_real (p, FF)
  else
     if (.not. allocated (amp_tree)) then
        n_total_hel = n_hel
        call allocate_amps ()
     end if
     amp_tree = zero
     amp_summed = zero
     if (threshold%settings%interference) then
        call threshold%formfactor%disable ()
        call full_proc_new_event (p)
        do hi = 1, full_proc_number_spin_states()
           amp_tree(hi) = full_proc_get_amplitude (1, hi, 1)
        end do
     end if
     call threshold%formfactor%activate ()
     call compute_born (p, FF)
     select case (FF)
     case (EXPANDED_HARD_P0DEPENDENT, EXPANDED_HARD_P0CONSTANT, &
             EXPANDED_SOFT_P0CONSTANT, EXPANDED_SOFT_SWITCHOFF_P0CONSTANT, &
             EXPANDED_SOFT_HARD_P0CONSTANT)
        amp2 = expanded_amp2 (amp_tree, amp_blob)
     case (MATCHED)
        amp_summed = amp_tree + amp_blob
        amp2 = real (sum (abs2 (amp_summed)))
        call compute_born (p, MATCHED_EXPANDED)
        amp2 = amp2 + expanded_amp2 (amp_tree, amp_blob)
     case default
        if (threshold%settings%interference) then
           amp2 = real (sum (abs2 (amp_tree + amp_blob)))
        else
           if (threshold%settings%helicity_approximated) then
              amp2 = real (sum (amp_blob))
           else
              amp2 = real (sum (abs2 (amp_blob)))
           end if
        end if
     end select
     if (test_ward)  amp2 = 0
  end if
  amp2 = amp2 * production_factors

contains

  subroutine allocate_amps ()
    allocate (amp_blob(n_total_hel))
    allocate (amp_tree(n_total_hel))
    allocate (amp_summed(n_total_hel))
  end subroutine allocate_amps

end subroutine @ID@_get_amp_squared
