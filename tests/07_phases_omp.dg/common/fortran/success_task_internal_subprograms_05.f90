! <testinfo>
! test_generator=config/mercurium-omp
! </testinfo>
MODULE MOO
    IMPLICIT NONE

    INTEGER :: A

    CONTAINS

        SUBROUTINE S
            IMPLICIT NONE

            !$OMP TASK SHARED(A)
                CALL FOO(A)
            !$OMP END TASK

            CONTAINS
                SUBROUTINE FOO(Y)
                    IMPLICIT NONE
                    INTEGER :: Y

                    A = A + 1
                END SUBROUTINE FOO
        END SUBROUTINE S

END MODULE MOO

PROGRAM MAIN
    USE MOO, ONLY : A, S
    IMPLICIT NONE

    A = 3
    CALL S
    !$OMP TASKWAIT

    IF (A /= 4) STOP 1
END PROGRAM MAIN
