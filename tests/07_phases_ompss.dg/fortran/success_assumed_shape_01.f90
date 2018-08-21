! <testinfo>
! test_generator=(config/mercurium-ompss "config/mercurium-ompss-2 openmp-compatibility")
! </testinfo>
SUBROUTINE FOO(X, Y, Z)
    INTEGER :: X(:)
    INTEGER :: Y(:)
    INTEGER :: Z(:)

    LOGICAL :: ERR

    ERR = .FALSE.

    !$OMP TASK PRIVATE(X) FIRSTPRIVATE(Y) SHARED(Z, ERR)
        X = -1
        ERR = ANY(Y /= 2)
        Y = -2
        ERR = ERR .OR. ANY(Z /= 3)
        Z = -3
    !$OMP END TASK
    !$OMP TASKWAIT

    IF (ERR) STOP -1
    IF (ANY(X /= 1))  STOP -2
    IF (ANY(Y /= 2))  STOP -3
    IF (ANY(Z /= -3)) STOP -4
END SUBROUTINE FOO

PROGRAM P
    IMPLICIT NONE
    INTEGER :: V1(10)
    INTEGER :: V2(10)
    INTEGER :: V3(10)

    INTERFACE
        SUBROUTINE FOO(X, Y, Z)
            INTEGER :: X(:)
            INTEGER :: Y(:)
            INTEGER :: Z(:)
        END SUBROUTINE FOO
    END INTERFACE

    V1 = 1
    V2 = 2
    V3 = 3
    CALL FOO(V1, V2, V3)
END PROGRAM P
