module @ID@_virtual
  use iso_c_binding
  use kinds
  use diagnostics
  use openloops
  use parameters_sm_tt_threshold
  implicit none
  integer, dimension(4) :: id
end module @ID@_virtual

subroutine @ID@_start_openloops () bind(C)
  use @ID@_virtual
  integer, dimension(12), parameter :: pdgs = [1,2,3,4,5,6,11,13,15,23,24,25]
  character(32) :: buffer
  integer :: i
  ! coupling order alpha_ew^1, implies QCD correction for loop process
  call set_parameter("order_ew", 1)
  call set_parameter("model", "sm")
  do i = 1, size(pdgs)
     if (pdgs(i) > 4) then
        write (buffer, "(I0)")  pdgs(i)
        call set_parameter("mass(" // trim (adjustl (buffer)) // ")", mass(pdgs(i)))
        call set_parameter("width(" // trim (adjustl (buffer)) // ")", width(pdgs(i)))
     end if
  end do
  call set_parameter("alpha", real(1 / alphaemi, kind=double))
  ! Increase verbosity level to list loaded libraries
  call set_parameter("verbose", 3)
  call set_parameter("psp_tolerance", 10e-7_double)
  call set_parameter("use_cms", 0)
  call set_parameter("me_cache", 0)
  ! 1 for tree-like matrix elements (tree, color and spin correlations),
  ! 11 for loop, 12 for loop^2
  id(1) = register_process("6(-1) -> 24 5", 11)
  id(2) = register_process("6(+1) -> 24 5", 11)
  !id(1) = register_process("6 -> 24 5", 11)
  !id(2) = register_process("6 -> 24 5", 11)
  !!! Anti-particles need opposite sign for the helicity
  !!! Different conventions in OMega and OpenLoops
  id(3) = register_process("-6(+1) -> -24 -5", 11)
  id(4) = register_process("-6(-1) -> -24 -5", 11)
  !id(3) = register_process("-6 -> -24 -5", 11)
  !id(4) = register_process("-6 -> -24 -5", 11)
  call start()
end subroutine @ID@_start_openloops

subroutine @ID@_olp_eval2 (i_flv, alpha_s_c, parray, mu_c, &
       sqme_c, acc_c) bind(C)
  use @ID@_threshold
  use @ID@_virtual
  use physics_defs, only: ass_boson, ass_quark
  use physics_defs, only: THR_POS_WP, THR_POS_WM
  use physics_defs, only: THR_POS_B, THR_POS_BBAR
  use ttv_formfactors
  use omega95
  implicit none
  integer(c_int), intent(in) :: i_flv
  real(c_default_float), intent(in) :: alpha_s_c
  real(c_default_float), dimension(0:3,*), intent(in) :: parray
  real(c_default_float), intent(in) :: mu_c
  real(c_default_float), dimension(4), intent(out) :: sqme_c
  real(c_default_float), intent(out) :: acc_c
  complex(default), dimension(-1:1,-1:1,-1:1,-1:1) :: production_me
  type(momentum), dimension(2) :: p_top
  integer :: h_el, h_pos, h_t, h_tbar
  integer :: leg, this_id, h_b, h_W
  real(double) :: virt_decay(0:3), total(0:3), acc
  real(double) :: p_decay(0:3,3,2)
  real(double) :: mu, alpha_s, dynamic_top_mass
  complex(default) :: born_decay_me, bw
  real(default) :: prod2, born_decay_me2
  call msg_debug (D_ME_METHODS, "@ID@_olp_eval2")
  if (i_flv /= 1)  call msg_fatal ("i_flv /= 1, threshold interface was not built for this")
  if (any (id <= 0))  call msg_fatal ("Could not register process in OpenLoops")
  if (.not. threshold%settings%factorized_computation)  &
       call msg_fatal ("@ID@_olp_eval2: OFFSHELL_STRATEGY is not factorized")
  alpha_s = alpha_s_c
  mu = mu_c
  call init_workspace ()
  call set_parameter("alpha_s", alpha_s)
  call set_parameter("mu", mu)
  total = 0
  do leg = 1, 2
     p_decay(:,2,leg) = parray(:,ass_boson(leg))
     p_decay(:,3,leg) = parray(:,ass_quark(leg))
     p_decay(:,1,leg) = p_decay(:,2,leg) + p_decay(:,3,leg)
     p_top(leg) = p_decay(:,1,leg)
  end do
  call compute_momentum_sums ()
  call set_top_momenta ()
  call set_parameter("width(6)", zero)
  call set_parameter("width(24)", zero)
  bw = top_propagators (FF)
  production_me = compute_production_me (FF)
  do h_t = -1, 1, 2
  do h_tbar = -1, 1, 2
     prod2 = 0
     do h_el = -1, 1, 2
     do h_pos = -1, 1, 2
        prod2 = prod2 + abs2 (production_me(h_el, h_pos, h_t, h_tbar))
     end do
     end do
     do leg = 1, 2
        dynamic_top_mass = sqrt (p_top(leg) * p_top(leg))
        call set_parameter("mass(6)", dynamic_top_mass)
        born_decay_me2 = zero
        do h_W = -1, 1
        do h_b = -1, 1, 2
           if (leg == 1) then
              born_decay_me = anti_top_decay_born (h_tbar, h_W, h_b)
              this_id = (3 + h_t) / 2
           else
              born_decay_me = top_decay_born (h_t, h_W, h_b)
              this_id = (3 + h_tbar) / 2 + 2
           end if
           born_decay_me2 = born_decay_me2 + abs2 (born_decay_me)
        end do
        end do
        ! TODO: (bcn 2016-01-22) handle acc
        call evaluate_loop (id(this_id), p_decay(:,:,leg), virt_decay(3), &
             virt_decay(0:2), acc)
        total = total + prod2 * born_decay_me2 * virt_decay * abs2 (bw)
     end do
  end do
  end do
  !!! OpenLoops applies the averaging factor of four regardless whether
  !!! the MEs are polarized or not
  total = total * production_factors * four
  sqme_c = [total(2), total(1), total(0), total(3)]
  if (debug2_active (D_ME_METHODS)) then
     print *, 'sqme_c =    ', sqme_c !!! Debugging
  end if
contains
  subroutine set_top_momenta ()
    mom_top_onshell = parray(:,THR_POS_WP) + parray(:,THR_POS_B)
    mom_topbar_onshell = parray(:,THR_POS_WM) + parray(:,THR_POS_BBAR)
    mom_wp_onshell = parray(:,THR_POS_WP)
    mom_wm_onshell = parray(:,THR_POS_WM)
    mom_b_onshell = parray(:,THR_POS_B)
    mom_bbar_onshell = parray(:,THR_POS_BBAR)
  end subroutine set_top_momenta
end subroutine @ID@_olp_eval2

subroutine @ID@_stop_openloops () bind(C)
  use iso_c_binding
  use openloops
  call finish()
end subroutine @ID@_stop_openloops
