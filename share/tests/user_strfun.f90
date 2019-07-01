subroutine escan_info (n_in, n_out, n_states, n_col, n_dim, n_var) bind(C)
  use iso_c_binding
  integer(c_int), intent(inout) :: n_in, n_out, n_states, n_col
  integer(c_int), intent(inout) :: n_dim, n_var
  n_in = 1
  n_out = 2
  n_states = 2
  n_col = 1
  n_dim = 1
  n_var = 0
end subroutine escan_info

subroutine escan_mask (i_prt, m_flv, m_col, m_hel, i_lock) bind(C)
  use iso_c_binding
  integer(c_int), intent(in) :: i_prt
  integer(c_int), intent(inout) :: m_flv, m_hel, m_col, i_lock
  select case (i_prt)
  case (1)
     i_lock = 3
  case (2)
     m_hel = 1
  case (3)
     i_lock = 1
  end select
end subroutine escan_mask

subroutine escan_state (i_state, i_prt, flv, hel, col) bind(C)
  use iso_c_binding
  integer(c_int), intent(in) :: i_state, i_prt
  integer(c_int), intent(inout) :: flv, hel
  integer(c_int), dimension(*), intent(inout) :: col
  select case (i_prt)
  case (1, 3)
     flv = 11
     select case (i_state)
     case (1);  hel = -1
     case (2);  hel = 1
     end select
  case (2)
     flv = 22
  end select
end subroutine escan_state
        
subroutine escan_kinematics (prt_in, rval, prt_out, xval) bind(C)
  use iso_c_binding
  use c_particles
  use kinds
  use lorentz
  type(c_prt_t), dimension(*), intent(in) :: prt_in
  real(c_double), dimension(*), intent(in) :: rval
  type(c_prt_t), dimension(*), intent(inout) :: prt_out
  real(c_double), dimension(*), intent(out) :: xval
  type(vector4_t), dimension(3) :: p
  real(default) :: x
  x = rval(1)**2
  p(1) = vector4_from_c_prt (prt_in(1))
  p(2) = (1-x) * p(1)
  p(3) = x * p(1)
  prt_out(1:2) = vector4_to_c_prt (p(2:3))
end subroutine escan_kinematics

subroutine escan_evaluate (xval, scale, fval) bind(C)
  use iso_c_binding
  real(c_double), dimension(*), intent(in) :: xval
  real(c_double), intent(in) :: scale
  real(c_double), dimension(*), intent(out) :: fval
  fval(1) = 1
  fval(2) = 1
end subroutine escan_evaluate
