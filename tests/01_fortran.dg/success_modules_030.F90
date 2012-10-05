! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
MODULE P
INTEGER, SAVE :: TAG = 220

CONTAINS

  SUBROUTINE S
     TAG = TAG + 1
  END SUBROUTINE S
  SUBROUTINE S2
     TAG = TAG + 1
  END SUBROUTINE S2

END MODULE P
