! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
PROGRAM P
  IMPLICIT NONE

  INTEGER ,PARAMETER :: idel = ICHAR('a')-ICHAR('A')

  PRINT *, idel

END PROGRAM
