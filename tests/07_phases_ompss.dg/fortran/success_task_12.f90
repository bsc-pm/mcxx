! <testinfo>
! test_generator=config/mercurium-ompss
! </testinfo>

MODULE MY_MOD
    IMPLICIT NONE
    TYPE FOO
        INTEGER :: Y
    END TYPE FOO
END MODULE MY_MOD

SUBROUTINE S(Z, N)
    USE MY_MOD, ONLY : FOO
    IMPLICIT NONE

    INTEGER :: N
    TYPE(FOO) :: Z(10)

    Z % Y = N
END SUBROUTINE S

SUBROUTINE S2(Z, N)
    USE MY_MOD, ONLY : FOO
    IMPLICIT NONE

    TYPE(FOO) :: Z(10)
    INTEGER :: N

    IF (ANY(Z % Y /= N)) STOP 2
END SUBROUTINE S2

PROGRAM MAIN
    USE MY_MOD, ONLY : FOO
    IMPLICIT NONE

    TYPE(FOO) :: X(10)

    INTERFACE
        !$OMP TASK INOUT(Z)
        SUBROUTINE S(Z, N)
            USE MY_MOD, ONLY : FOO
            IMPLICIT NONE

            TYPE(FOO) :: Z(10)
            INTEGER :: N
        END SUBROUTINE S
        !$OMP TASK IN(Z)
        SUBROUTINE S2(Z, N)
            USE MY_MOD, ONLY : FOO
            IMPLICIT NONE

            TYPE(FOO) :: Z(10)
            INTEGER :: N
        END SUBROUTINE S2
    END INTERFACE

    CALL S(X, 4)
    !$OMP TASKWAIT
    IF (ANY(X % Y /= 4)) STOP 1

    CALL S(X, 12)
    CALL S2(X, 12)
    !$OMP TASKWAIT

END PROGRAM MAIN
