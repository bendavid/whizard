! Dummy replacement routines

subroutine stdxwinit (filename, title, ntries, istream, lok)
  integer, intent(in) :: istream, lok
  integer, intent(in) :: ntries
  character(*), intent(in) :: filename, title
  write (0, "(A)")  "*************************************************************"
  write (0, "(A)")  "*** STDHEP: Error: library not linked, WHIZARD terminates ***"
  write (0, "(A)")  "*************************************************************"
  stop
end subroutine stdxwinit

subroutine stdxwrt (ilbl, istr, lok)
  integer, intent(in) :: ilbl
  integer, intent(in) :: istr, lok
  write (0, "(A)")  "*************************************************************"
  write (0, "(A)")  "*** STDHEP: Error: library not linked, WHIZARD terminates ***"
  write (0, "(A)")  "*************************************************************"
  stop
end subroutine stdxwrt

subroutine stdxend (istr)
  integer, intent(in) :: istr
  write (0, "(A)")  "*************************************************************"
  write (0, "(A)")  "*** STDHEP: Error: library not linked, WHIZARD terminates ***"
  write (0, "(A)")  "*************************************************************"
  stop
end subroutine stdxend
