! <testinfo>
! test_generator=config/mercurium-omp
! </testinfo>
MODULE FOO
  TYPE, PRIVATE:: T
    INTEGER :: X
  END TYPE

  CONTAINS

  SUBROUTINE SUB

     TYPE(T) :: S
     S % X = 1
     CALL INNER
     !$OMP TASKWAIT
     IF (S % X /= 3) STOP 1

     CONTAINS
        SUBROUTINE INNER
           !$OMP TASK SHARED(S)
           S % X = 3
           !$OMP END TASK
        END SUBROUTINE INNER
  END SUBROUTINE SUB
END MODULE FOO

PROGRAM MAIN
    USE FOO

    CALL SUB
END PROGRAM MAIN
