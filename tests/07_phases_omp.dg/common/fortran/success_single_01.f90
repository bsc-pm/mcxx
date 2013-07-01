! <testinfo>
! test_generator=config/mercurium-omp
! </testinfo>
PROGRAM MAIN
    IMPLICIT NONE
    INTEGER :: X, Y, Z

    X = 1
    Y = 42
    Z = 1

    !$OMP SINGLE PRIVATE(X) FIRSTPRIVATE(Y)
        x = 99
        if (y /= 42) STOP 1
        y = 99;
        z = 99;
    !$OMP END SINGLE

    if (x /= 1) STOP 2

    if (y /= 42) STOP 3

    if (z /= 99) STOP 4
END PROGRAM MAIN
