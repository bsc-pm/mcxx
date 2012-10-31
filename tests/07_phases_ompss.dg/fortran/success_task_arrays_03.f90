! <testinfo>
! test_generator=config/mercurium-ompss
! </testinfo>
MODULE FOO
    IMPLICIT NONE

CONTAINS

    !$OMP TASK INOUT(A(N:M))
    SUBROUTINE S(A, N, M)
        IMPLICIT NONE
        INTEGER :: N, M
        INTEGER :: A(:)

        PRINT "(I0,':',I0,' Range: ',I0, '->', I0)", LBOUND(A, DIM=1), UBOUND(A, DIM=1), N, M
        A = A + 1
    END SUBROUTINE S

END MODULE FOO

SUBROUTINE S2(B)
    USE FOO, ONLY : S
    IMPLICIT NONE

    INTEGER :: B(:)

    CALL S(B, 2, 30)
END SUBROUTINE S2

PROGRAM MAIN
    IMPLICIT NONE
    INTEGER :: C(200)

    INTERFACE
        SUBROUTINE S2(B)
            IMPLICIT NONE
            INTEGER :: B(:)
        END SUBROUTINE S2
    END INTERFACE

    C = 3
    CALL S2(C)
    !$OMP TASKWAIT

    IF (ANY(C(2:30) /= 4)) STOP 1

    PRINT *, "ALL OK!"
END PROGRAM MAIN
