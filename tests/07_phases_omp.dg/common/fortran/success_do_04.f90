! <testinfo>
! test_generator=config/mercurium-omp
! </testinfo>
PROGRAM P
    IMPLICIT NONE
    INTEGER :: I
    INTEGER :: X(10)
    X = 0
    !$OMP PARALLEL DO
    DO I=1, 10
        X(I) = FOO(I)
    END DO

    !$OMP PARALLEL DO
    DO I=1,10
    if (X(I) /= I + 1) STOP 2
    END DO

    CONTAINS

        FUNCTION FOO(I)
            IMPLICIT NONE
            INTEGER :: I
            INTEGER :: FOO

            FOO = I + 1
        END FUNCTION FOO

END PROGRAM P
