module @ID@_threshold
  use kinds
  use omega95
  use parameters_SM_tt_threshold
  use ttv_formfactors
  implicit none
  private
  public :: init, md5sum, calculate_blobs

  ! DON'T EVEN THINK of removing the following!
  ! If the compiler complains about undeclared
  ! or undefined variables, you are compiling
  ! against an incompatible omega95 module!
  integer, dimension(7), parameter, private :: require = &
    (/ omega_spinors_2010_01_A, omega_spinor_cpls_2010_01_A, &
       omega_vectors_2010_01_A, omega_polarizations_2010_01_A, &
       omega_couplings_2010_01_A, omega_color_2010_01_A, &
       omega_utils_2010_01_A /)

  integer, parameter, public :: n_prt = 6
  integer, parameter :: n_prt_OS = 4
  integer, parameter, public :: n_in = 2
  integer, parameter, public :: n_out = 4
  integer, parameter, public :: n_cflow = 1
  integer, parameter, public :: n_cindex = 2
  integer, parameter, public :: n_flv = 1
  integer, parameter, public :: n_hel = 144
  integer, parameter :: n_hel_OS = 16

  ! NB: you MUST NOT change the value of N_ here!!!
  !     It is defined here for convenience only and must be
  !     compatible with hardcoded values in the amplitude!
  real(kind=default), parameter, public :: N_ = 3
  logical, parameter :: F = .false.
  logical, parameter :: T = .true.

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

  complex(default), dimension(n_hel,0:3), save, public :: amp_blob
  complex(default), dimension(n_hel), save, public :: amp_tree

  type(momentum) :: p1, p2, p3, p4, p5, p6
  type(momentum) :: p12, p35, p46
  type(spinor) :: owf_t_4, owf_b_6, owf_e_1
  type(conjspinor) :: owf_t_3, owf_b_5, owf_e_2
  type(vector) :: owf_Wm_3, owf_Wp_4
  type(spinor) :: owf_wb_46
  type(conjspinor) :: owf_wb_35
  type(vector) :: owf_A_12, owf_Z_12

contains

  pure function md5sum ()
    character(len=32) :: md5sum
    ! DON'T EVEN THINK of modifying the following line!
    md5sum = "3C3BACDC1B40C40DB783E084F0B62DFC"
  end function md5sum

  subroutine init (par)
    real(kind=default), dimension(*), intent(in) :: par
    call import_from_whizard (par)
  end subroutine init

  subroutine calculate_blobs (k)
    real(kind=default), dimension(0:3,*), intent(in) :: k
    complex(default) :: blob_Z_vec, blob_Z_ax, ttv_vec, ttv_ax
    integer, dimension(n_prt) :: s
    integer, dimension(n_prt_OS) :: s_OS
    integer :: hi, ffi_end, ffi
    integer, dimension(0:3) :: ff_modes
    p1 = - k(:,1) ! incoming
    p2 = - k(:,2) ! incoming
    p3 =   k(:,3) ! outgoing
    p4 =   k(:,4) ! outgoing
    p5 =   k(:,5) ! outgoing
    p6 =   k(:,6) ! outgoing
    p12 = p1 + p2
    p35 = p3 + p5
    p46 = p4 + p6
    amp_blob = zero
    ff_modes(0:3) = [FF, EXPANDED_HARD_P0CONSTANT, EXPANDED_SOFT_HARD_P0CONSTANT, &
                     EXPANDED_SOFT_SWITCHOFF_P0CONSTANT]
    if (onshell_tops (p3, p4)) then
       do hi = 1, n_hel_OS
          s_OS = table_spin_states_OS(:,hi)
          owf_e_1 = u (mass(11), - p1, s_OS(1))
          owf_e_2 = vbar (mass(11), - p2, s_OS(2))
          owf_t_3 = ubar (ttv_mtpole(p12*p12), p3, s_OS(3))
          owf_t_4 = v (ttv_mtpole(p12*p12), p4, s_OS(4))

          owf_A_12 = pr_feynman (p12, v_ff (qlep, owf_e_2, owf_e_1))
          owf_Z_12 = pr_unitarity (p12, mass(23), wd_tl (p12, width(23)), &
               + va_ff (gnclep(1), gnclep(2), owf_e_2, owf_e_1))

          if (FF == MATCHED) then
             ffi_end = 3
          else
             ffi_end = 0
          end if
          do ffi = 0, ffi_end
             blob_Z_vec = gncup(1) * ttv_formfactor (p3, p4, 1)
             blob_Z_ax = gncup(2) * ttv_formfactor (p3, p4, 2)
             amp_blob(hi,ffi) = owf_Z_12 * &
                  va_ff (blob_Z_vec, blob_Z_ax, owf_t_3, owf_t_4)
             amp_blob(hi,ffi) = amp_blob(hi,ffi) + owf_A_12 * &
                  v_ff (qup, owf_t_3, owf_t_4) * ttv_formfactor (p3, p4, 1)
          end do
       end do
    else
       do hi = 1, n_hel
          s = table_spin_states(:,hi)
          owf_e_1 = u (mass(11), - p1, s(1))
          owf_e_2 = vbar (mass(11), - p2, s(2))
          owf_Wm_3 = conjg (eps (mass(24), p3, s(3)))
          owf_Wp_4 = conjg (eps (mass(24), p4, s(4)))
          owf_b_5 = ubar (mass(5), p5, s(5))
          owf_b_6 = v (mass(5), p6, s(6))

          owf_A_12 = pr_feynman(p12, v_ff (qlep, owf_e_2, owf_e_1))
          owf_Z_12 = pr_unitarity(p12, mass(23), wd_tl (p12, width(23)), &
               + va_ff (gnclep(1), gnclep(2), owf_e_2, owf_e_1))

          owf_wb_35 = pr_psibar (p35, ttv_mtpole (p12*p12), wd_tl (p35, width(6)), &
               + f_fvl (gccq33, owf_b_5, owf_Wm_3))
          owf_wb_46 = pr_psi (p46, ttv_mtpole(p12*p12), wd_tl (p46, width(6)), &
               + f_vlf (gccq33, owf_Wp_4, owf_b_6))

          if (FF == MATCHED) then
             ffi_end = 3
          else
             ffi_end = 0
          end if
          do ffi = 0, ffi_end
             ttv_vec = ttv_formfactor (p35, p46, 1, ff_modes(ffi))
             ttv_ax = ttv_formfactor (p35, p46, 2, ff_modes(ffi))
             blob_Z_vec = gncup(1) * ttv_vec
             blob_Z_ax = gncup(2) * ttv_ax
             amp_blob(hi,ffi) = owf_Z_12 * &
                  va_ff (blob_Z_vec, blob_Z_ax, owf_wb_35, owf_wb_46)
             amp_blob(hi,ffi) = amp_blob(hi,ffi) + owf_A_12 * &
                  v_ff (qup, owf_wb_35, owf_wb_46) * ttv_vec
          end do
       end do
    end if
    amp_blob = - amp_blob ! 4 vertices, 3 propagators
  end subroutine calculate_blobs

end module @ID@_threshold

! alphas will be set in ttv_formfactors
! warning: this only works with SM_tt_threshold. As this model will
!        also be used for the full diagrams, we should disable the
!        va_ilc_tta/z there
subroutine threshold_init (par) bind(C)
  use iso_c_binding
  use kinds
  use @ID@_threshold
  implicit none
  real(c_default_float), dimension(*), intent(in) :: par
  call init (par)
end subroutine threshold_init

!subroutine @ID@_threshold_get_amplitude_squared (p) bind(C)
subroutine threshold_get_amp_squared (amp2, p) bind(C)
  use iso_c_binding
  use kinds
  use opr_@ID@, sm_new_event => new_event
  use opr_@ID@, sm_get_amplitude => get_amplitude
  use opr_@ID@, sm_number_spin_states => number_spin_states
  use @ID@_threshold
  use parameters_sm_tt_threshold
  use ttv_formfactors
  use constants
  implicit none
  real(c_default_float), intent(out) :: amp2
  real(default), dimension(0:3) :: amp2_array
  real(c_default_float), dimension(0:3,*), intent(in) :: p
  integer, dimension(0:3) :: signs
  complex(default) :: amp_sm
  integer :: i, hi
  amp_tree = zero
  USE_FF = .false.
  call sm_new_event (p)
  do hi = 1, sm_number_spin_states()
     amp_tree(hi) = sm_get_amplitude (1, hi, 1)
  end do
  USE_FF = .true.
  call calculate_blobs (p)
  select case (FF)
  case (EXPANDED_HARD_P0DEPENDENT, EXPANDED_HARD_P0CONSTANT, &
          EXPANDED_SOFT_P0CONSTANT, EXPANDED_SOFT_SWITCHOFF_P0CONSTANT, &
          EXPANDED_SOFT_HARD_P0CONSTANT)
     amp2 = expanded_amp2 (amp_tree, amp_blob(:,0))
  case (MATCHED)
     signs(0:3) = [+1, -1, +1, -1]
     amp2_array(0) = sum ((amp_tree + amp_blob(:,0)) * &
          conjg (amp_tree + amp_blob(:,0)))
     do i = 1, 3
        amp2_array(i) = expanded_amp2 (amp_tree, amp_blob(:,i))
     end do
     amp2 = sum (signs * amp2_array)
  case default
     amp2 = sum ((amp_tree + amp_blob(:,0)) * conjg (amp_tree + amp_blob(:,0)))
  end select
  amp2 = amp2 * N_ / 4.0_default
end subroutine threshold_get_amp_squared
!end subroutine @ID@_threshold_get_amplitude_squared
