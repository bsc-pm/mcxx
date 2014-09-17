! <testinfo>
! test_generator=config/mercurium-omp
! </testinfo>
PROGRAM MAIN
    IMPLICIT NONE
    INTEGER :: X, I

    INTEGER :: A1, A2, A3

    A1 = -1
    A2 = -2
    A3 = -3

    !$OMP PARALLEL SECTIONS
    !$OMP SECTION
    A1 = -A1
    !$OMP SECTION
    A2 = -A2
    !$OMP SECTION
    A3 = -A3
    !$OMP END PARALLEL SECTIONS

    IF (A1 /= 1 .OR. A2 /= 2 .OR. A3 /= 3) STOP 1
END PROGRAM MAIN
