! <testinfo>
! test_generator=config/mercurium-omp
! </testinfo>
PROGRAM OUTER
    IMPLICIT NONE
    INTEGER :: Y(100), J, M, W

    !$OMP PARALLEL
    CALL INNER(Y, 100, M)
    !$OMP END PARALLEL

    W = 0
    DO J = 1, 100
        IF (Y(J) /= J) STOP 1
        W = W + J
    END DO

    IF (W /= M) STOP 2

    CONTAINS

        SUBROUTINE INNER(X, N, S)
            IMPLICIT NONE
            INTEGER :: N, X(N), I, S

            S = 0

            !$OMP DO REDUCTION(+:S)
            DO I = 1, N
                X(I) = I
                S = S + I
            END DO
            !$OMP END DO

        END SUBROUTINE INNER
END PROGRAM OUTER
