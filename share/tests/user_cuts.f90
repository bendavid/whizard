function ptcut (prt, n_prt) result (iflag) bind(C)
  use iso_c_binding
  use c_particles
  use lorentz
  type(c_prt_t), dimension(*), intent(in) :: prt
  integer(c_int), intent(in) :: n_prt
  integer(c_int) :: iflag
  logical, save :: first = .true.
  if (all (transverse_part (vector4_from_c_prt (prt(1:n_prt))) > 50)) then
     iflag = 1
  else
     iflag = 0
  end if
end function ptcut

function pt1 (prt, n_prt) result (rval) bind(C)
  use iso_c_binding
  use c_particles
  use lorentz
  type(c_prt_t), dimension(*), intent(in) :: prt
  integer(c_int), intent(in) :: n_prt
  real(c_double) :: rval
  rval = transverse_part (vector4_from_c_prt (prt(1)))
end function pt1

function ptval (prt1) result (rval) bind(C)
  use iso_c_binding
  use c_particles
  use lorentz
  type(c_prt_t), intent(in) :: prt1
  real(c_double) :: rval
  rval = transverse_part (vector4_from_c_prt (prt1))
end function ptval
