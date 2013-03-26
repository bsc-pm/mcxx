! <testinfo>
! test_generator=config/mercurium-omp
! </testinfo>
PROGRAM MAIN
    IMPLICIT NONE
    INTEGER :: A

    A = 1
    !$OMP PARALLEL DEFAULT(PRIVATE)
    A = 3
    !$OMP END PARALLEL

    IF (A /= 1) STOP 1

END PROGRAM MAIN
