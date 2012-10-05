! <testinfo>
! test_generator=config/mercurium-omp
! </testinfo>
SUBROUTINE S(X)
    IMPLICIT NONE
    INTEGER, OPTIONAL :: X

    !$OMP TASK SHARED(X)
    IF (PRESENT(X)) THEN
      X = X + 1
    END IF
    !$OMP END TASK
    !$OMP TASKWAIT
END SUBROUTINE S

PROGRAM P
    IMPLICIT NONE
    INTERFACE
        SUBROUTINE S(X)
            IMPLICIT NONE
            INTEGER, OPTIONAL :: X
        END SUBROUTINE S
    END INTERFACE

    INTEGER :: M

    M = 1
    CALL S()
    IF (M /= 1) STOP 1

    CALL S(M)
    IF (M /= 2) STOP 2
END PROGRAM P
