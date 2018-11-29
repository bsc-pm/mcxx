! <testinfo>
! test_generator=(config/mercurium-ompss "config/mercurium-ompss-2 openmp-compatibility")
! </testinfo>
SUBROUTINE FOO(X)
    IMPLICIT NONE
    INTEGER :: X(:)
    INTEGER,ALLOCATABLE :: Y(:)
    INTEGER, POINTER :: Z(:)

    ALLOCATE(Y(10))
    ALLOCATE(Z(10))

    X=0
    Y=0
    Z=0
    !$OMP TASK PRIVATE(X, Y, Z)
        X=1
        Y=1
    !$OMP END TASK
    !$OMP TASKWAIT

    IF (ANY(X/=0) .OR. ANY(Y/=0) .OR. ANY(Z/=0)) THEN
        STOP -1
    ENDIF
END SUBROUTINE FOO

PROGRAM P
    IMPLICIT NONE
    INTEGER :: V(10)
    INTERFACE
        SUBROUTINE FOO(X)
            IMPLICIT NONE
            INTEGER :: X(:)
        END SUBROUTINE FOO
    END INTERFACE
    CALL FOO(V)
END PROGRAM P
