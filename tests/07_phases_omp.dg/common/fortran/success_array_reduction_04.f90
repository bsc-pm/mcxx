! <testinfo>
! test_generator=config/mercurium-omp
! </testinfo>
SUBROUTINE S(X)
    INTEGER :: X
    INTEGER :: C(X)

    C = 0

    !$OMP PARALLEL DO REDUCTION(+:C)
    DO I = 1, 100
        C = C + I
    END DO
    !$OMP END PARALLEL DO

    IF (C(1) /= 5050) STOP 1
END SUBROUTINE S

PROGRAM MAIN
    CALL S(10)
END PROGRAM MAIN
