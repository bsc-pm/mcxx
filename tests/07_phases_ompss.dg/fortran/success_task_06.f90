! <testinfo>
! test_generator=config/mercurium-ompss
! </testinfo>
PROGRAM MAIN
    IMPLICIT NONE
    INTERFACE
        !$OMP TASK INOUT(X)
        SUBROUTINE T(X)
            IMPLICIT NONE
            INTEGER :: X
        END SUBROUTINE T
    END INTERFACE

    TYPE MY_TYPE
        INTEGER :: X
    END TYPE MY_TYPE

    TYPE(MY_TYPE) :: MT
    INTEGER :: A

    A = 1

    CALL T(A)
    !$OMP TASKWAIT

    IF (A /= 2) STOP 1
END PROGRAM MAIN

SUBROUTINE T(X)
    INTEGER :: X
    X = X + 1
END SUBROUTINE T
