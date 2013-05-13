! <testinfo>
! test_generator=config/mercurium-omp
! </testinfo>
SUBROUTINE S
    IMPLICIT NONE
    INTEGER :: X

    CALL FOO

    CONTAINS
        SUBROUTINE FOO
            !$OMP PARALLEL
            X = 1
            !$OMP END PARALLEL
        END SUBROUTINE FOO

END SUBROUTINE S

SUBROUTINE Q
    IMPLICIT NONE
    INTEGER :: X

    CALL FOO

    CONTAINS
        SUBROUTINE FOO
            !$OMP PARALLEL
            X = 1
            !$OMP END PARALLEL
        END SUBROUTINE FOO

END SUBROUTINE Q

PROGRAM MAIN
    IMPLICIT NONE
    CALL S
    CALL Q
END PROGRAM MAIN
