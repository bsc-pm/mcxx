! <testinfo>
! test_generator=config/mercurium-omp
! </testinfo>
PROGRAM MAIN
    IMPLICIT NONE
    INTEGER, PARAMETER :: NA = 100
    INTEGER, PARAMETER :: NB = 200
    INTEGER :: A(NA), B(NA, NB)
    INTEGER :: S
    INTEGER :: I

    S = 0
    DO I = 1, NB
        B(:, I) = I
        S = S + I
    END DO

    A(:) = 0

    !$OMP PARALLEL DO REDUCTION(+:A)
    DO I = 1, NB
        A = A + B(:, I)
    END DO
    !$OMP END PARALLEL DO

    DO I = 1, NA
       IF (A(I) /= S) THEN
           PRINT *, "ERROR: I = ", I, "A(I)=", A(I)
           STOP 1
       END IF
    END DO
END PROGRAM MAIN
