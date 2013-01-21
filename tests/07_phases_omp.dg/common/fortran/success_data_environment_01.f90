! <testinfo>
! test_generator=config/mercurium-omp
! </testinfo>
PROGRAM MAIN
    IMPLICIT NONE
    INTEGER :: X, I

    DATA X /0/

    !$OMP PARALLEL
    !$OMP DO
    DO I = 1, 100
       !$OMP ATOMIC
       X = X + I
    END DO
    !$OMP END DO
    !$OMP END PARALLEL

    PRINT *, X
    IF (X /= 5050) STOP 1
END
