! <testinfo>
! test_generator=config/mercurium-omp
! </testinfo>
PROGRAM MAIN
    IMPLICIT NONE
    INTEGER :: JARL, I

    !$OMP PARALLEL PRIVATE(JARL)

    !$OMP DO
    DO I = 1, 100
        JARL = 2
    END DO

    !$OMP END PARALLEL

END PROGRAM MAIN
