! <testinfo>
! test_generator=config/mercurium-omp
! </testinfo>
PROGRAM MAIN
    IMPLICIT NONE
    INTEGER :: X, Z

    X = 1
    Z = 1

    !$OMP TASK SHARED(Z)
    CALL S(1)
    Z = Z ! Workaround a bug at the time of writing this test
    !$OMP END TASK

    !$OMP TASKWAIT

    IF (X /= 2) STOP 1
    IF (Z /= 2) STOP 2

    CONTAINS
        SUBROUTINE S(Y)
            IMPLICIT NONE
            INTEGER :: Y
            X = 1 + Y
            Z = 1 + Y
        END SUBROUTINE S
END PROGRAM MAIN

