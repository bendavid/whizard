! WHIZARD library manager
!
! This file handles the prebuilt process libraries
! (currently: empty)

function libmanager_get_n_libs () result (n)
  integer :: n
  n = 0
end function libmanager_get_n_libs

function libmanager_get_libname (i) result (name)
  use iso_varying_string, string_t => varying_string
  type(string_t) :: name
  integer, intent(in) :: i
  select case (i)
  case default;  name = ''
  end select
end function libmanager_get_libname

function libmanager_get_c_funptr (libname, fname) result (c_fptr)
  use iso_c_binding
  use prclib_interfaces
  type(c_funptr) :: c_fptr
  character(*), intent(in) :: libname, fname
  select case (libname)
  case default
     c_fptr = c_null_funptr
  end select
end function libmanager_get_c_funptr
