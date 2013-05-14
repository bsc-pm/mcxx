! <testinfo>
! test_generator=config/mercurium-omp
! </testinfo>
MODULE VARS
  IMPLICIT NONE
  PRIVATE

  PUBLIC :: LALA
  PUBLIC :: FOO

  TYPE FOO
    INTEGER :: X
  END TYPE FOO

  TYPE(FOO) :: LALA

END MODULE VARS

PROGRAM MAIN
        USE VARS, ONLY : LALA
        IMPLICIT NONE

        LALA % X = 1

        !$OMP TASK
        LALA % X = 3
        !$OMP END TASK

        !$OMP TASKWAIT

        IF (LALA % X /= 3) STOP 1
END PROGRAM MAIN
