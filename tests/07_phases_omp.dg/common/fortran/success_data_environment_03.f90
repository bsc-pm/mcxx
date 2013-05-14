! <testinfo>
! test_generator=config/mercurium-omp
! </testinfo>
MODULE TYPES
        IMPLICIT NONE
   TYPE FOO
    INTEGER :: X
   END TYPE FOO
END MODULE TYPES

MODULE VARS
  USE TYPES, ONLY: FOO
  IMPLICIT NONE

  TYPE(FOO) :: LDC
END MODULE VARS

PROGRAM MAIN
        USE VARS, ONLY : LDC
        IMPLICIT NONE

        LDC % X = 1

        !$OMP TASK
        LDC % X = 12
        !$OMP END TASK

        !$OMP TASKWAIT

        IF (LDC % X /= 12) STOP 1
END PROGRAM MAIN

