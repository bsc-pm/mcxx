! <testinfo>
! test_generator=(config/mercurium-ompss "config/mercurium-ompss-2 openmp-compatibility")
! </testinfo>
MODULE TYPES
    TYPE T
        INTEGER :: X
    END TYPE T
END MODULE TYPES

MODULE FOO
    USE TYPES, ONLY : T
    IMPLICIT NONE
    CONTAINS

        SUBROUTINE S
            IMPLICIT NONE
            TYPE(T) :: M

            M % X = 1
            CALL TASK(M)
            CALL TASK(M)
            CALL TASK(M)
            !$OMP TASKWAIT
            IF (M % X /= 4) STOP 1
        END SUBROUTINE S

        !$OMP TASK INOUT(X)
        SUBROUTINE TASK(X)
            IMPLICIT NONE
            TYPE(T) :: X

            X % X = X % X + 1
        END SUBROUTINE TASK
END MODULE FOO

PROGRAM MAIN
    USE FOO
    IMPLICIT NONE

    CALL S
END PROGRAM MAIN
