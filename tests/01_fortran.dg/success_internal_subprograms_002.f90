! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
PROGRAM P
INTEGER, SAVE :: TAG = 220

  CALL S
  CALL S2

CONTAINS

  SUBROUTINE S
     TAG = TAG + 1
  END SUBROUTINE S

  SUBROUTINE S2
     TAG = TAG + 1
  END SUBROUTINE S2

END PROGRAM P
