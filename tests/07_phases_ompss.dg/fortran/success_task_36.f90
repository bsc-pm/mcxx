! <testinfo>
! test_generator=config/mercurium-ompss
! </testinfo>
MODULE MOO
    INTEGER :: X
END MODULE MOO

SUBROUTINE SUB(Y)
    USE MOO, ONLY : X
    IMPLICIT NONE

    INTEGER :: Y

    !$OMP TASK
    CALL FOO
    !$OMP END TASK
    !$OMP TASKWAIT

    CONTAINS

        SUBROUTINE FOO
            INTEGER :: A(X+1)

            A = A + 1
            IF (Y+1 /= SIZE(A, DIM=1)) STOP 1
        END SUBROUTINE FOO
END SUBROUTINE SUB

PROGRAM MAIN
    USE MOO, ONLY : X
    IMPLICIT NONE

    X = 64

    CALL SUB(64)
END PROGRAM MAIN
