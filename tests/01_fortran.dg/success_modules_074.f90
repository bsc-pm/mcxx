! <testinfo>
! test_generator="config/mercurium-fortran run"
! </testinfo>
MODULE MOO

INTEGER, SAVE :: R(3)
DATA R / 1, 2, 3 /

END MODULE MOO

PROGRAM MAIN
  USE MOO
  IMPLICIT NONE

  IF ( ANY(R /= (/ 1, 2, 3 /)) ) STOP 1

END PROGRAM MAIN
