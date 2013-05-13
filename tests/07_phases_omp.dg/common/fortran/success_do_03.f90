! <testinfo>
! test_generator=config/mercurium-omp
! </testinfo>
PROGRAM MAIN
    IMPLICIT NONE
    INTEGER :: I, J

    !$OMP PARALLEL DEFAULT(PRIVATE)
    !$OMP DO
    DO I = 1, 100
        DO J = 1, I
             CALL S()
        END DO
    END DO
    !$OMP END DO
    !$OMP END PARALLEL
END PROGRAM MAIN

SUBROUTINE S
END SUBROUTINE S
