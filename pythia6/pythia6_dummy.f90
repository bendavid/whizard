subroutine pylist (i)
  integer, intent(in) :: i
end subroutine pylist

subroutine pyinit (frame, beam, target, win)
  character*(*), intent(in) ::  frame, beam, target
  double precision, intent(in) :: win
  write (0, "(A)")  "**************************************************************"
  write (0, "(A)")  "*** Error: PYTHIA6 has not been enabled, WHIZARD terminates ***"
  write (0, "(A)")  "**************************************************************"
  stop  
end subroutine pyinit
subroutine pygive (chin)
  character chin*(*)
  write (0, "(A)")  "**************************************************************"
  write (0, "(A)")  "*** Error: PYTHIA6 has not been enabled, WHIZARD terminates ***"
  write (0, "(A)")  "**************************************************************"
  stop  
end subroutine pygive
subroutine pyevnt()
  write (0, "(A)")  "**************************************************************"
  write (0, "(A)")  "*** Error: PYTHIA6 has not been enabled, WHIZARD terminates ***"
  write (0, "(A)")  "**************************************************************"
  stop  
end subroutine pyevnt
subroutine pyexec()
  write (0, "(A)")  "**************************************************************"
  write (0, "(A)")  "*** Error: PYTHIA6 has not been enabled, WHIZARD terminates ***"
  write (0, "(A)")  "**************************************************************"
  stop   
end subroutine pyexec
function pyp(I,J)
  integer, intent(in) :: i,j
  double precision :: pyp
  write (0, "(A)")  "**************************************************************"
  write (0, "(A)")  "*** Error: PYTHIA6 has not been enabled, WHIZARD terminates ***"
  write (0, "(A)")  "**************************************************************"
  stop  
end function pyp
subroutine pystop ()
  write (0, "(A)")  "**************************************************************"
  write (0, "(A)")  "*** Error: PYTHIA6 has not been enabled, WHIZARD terminates ***"
  write (0, "(A)")  "**************************************************************"
  stop  
end subroutine pystop


