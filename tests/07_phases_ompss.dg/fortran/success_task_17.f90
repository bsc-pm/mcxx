! <testinfo>
! test_generator=config/mercurium-ompss
! </testinfo>

SUBROUTINE FOO(X)
    IMPLICIT NONE
    INTEGER, VALUE :: X
    X = 2
END SUBROUTINE FOO

PROGRAM P
    IMPLICIT NONE
    INTERFACE
        !$OMP TASK
        SUBROUTINE FOO(X)
        IMPLICIT NONE
        INTEGER, VALUE :: X
        END SUBROUTINE FOO
    END INTERFACE

    INTEGER :: X
    X = 0

    CALL FOO(X)
    !$OMP TASKWAIT

    if ( X /= 0) STOP 1
END PROGRAM P
