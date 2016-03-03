! <testinfo>
! test_generator="config/mercurium-ompss"
! test_compile_fail_nanos6_mercurium=yes
! test_compile_fail_nanos6_imfc=yes
! </testinfo>
PROGRAM P
    IMPLICIT NONE
    INTEGER :: I
    INTEGER :: X(10)
    X = 0
    !$OMP DO
    DO I=1, 10
        X(I) = FOO(I)
    END DO

    !$OMP DO
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
