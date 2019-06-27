! Dummy replacement routines

SUBROUTINE PYLIST (I)
  INTEGER, INTENT(IN) :: I
  WRITE (0, "(A)")  "*************************************************************"
  WRITE (0, "(A)")  "*** PYTHIA: Error: library not linked, WHIZARD terminates ***"
  WRITE (0, "(A)")  "*************************************************************"
  stop
END SUBROUTINE PYLIST

SUBROUTINE PYHEPC (I)
  INTEGER, INTENT(IN) :: I
  WRITE (0, "(A)")  "*************************************************************"
  WRITE (0, "(A)")  "*** PYTHIA: Error: library not linked, WHIZARD terminates ***"
  WRITE (0, "(A)")  "*************************************************************"
  STOP
END SUBROUTINE PYHEPC

SUBROUTINE PYINIT(FRAME,BEAM,TARGET,WIN)
  CHARACTER*(*), INTENT(IN) ::  FRAME,BEAM,TARGET
  DOUBLE PRECISION, INTENT(IN) :: WIN
  WRITE (0, "(A)")  "*************************************************************"
  WRITE (0, "(A)")  "*** PYTHIA: Error: library not linked, WHIZARD terminates ***"
  WRITE (0, "(A)")  "*************************************************************"
  STOP
END SUBROUTINE PYINIT

SUBROUTINE PYGIVE(CHIN)
  CHARACTER CHIN*(*)
  WRITE (0, "(A)")  "*************************************************************"
  WRITE (0, "(A)")  "*** PYTHIA: Error: library not linked, WHIZARD terminates ***"
  WRITE (0, "(A)")  "*************************************************************"
  STOP
END SUBROUTINE PYGIVE

SUBROUTINE PYEVNT()
  WRITE (0, "(A)")  "*************************************************************"
  WRITE (0, "(A)")  "*** PYTHIA: Error: library not linked, WHIZARD terminates ***"
  WRITE (0, "(A)")  "*************************************************************"
  STOP
END SUBROUTINE PYEVNT
