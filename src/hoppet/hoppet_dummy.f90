! Dummy replacement routines

subroutine InitForWhizard ()
  write (0, "(A)")  "*************************************************************"
  write (0, "(A)")  "*** HOPPET: Error: library not linked, WHIZARD terminates ***"
  write (0, "(A)")  "*************************************************************"
  stop
end subroutine InitForWhizard

subroutine EvalForWhizard (x, q, f)
double precision, intent(in)  :: x, q
double precision, intent(out) :: f(-6:6)
  write (0, "(A)")  "*************************************************************"
  write (0, "(A)")  "*** HOPPET: Error: library not linked, WHIZARD terminates ***"
  write (0, "(A)")  "*************************************************************"
  stop
end subroutine EvalForWhizard
