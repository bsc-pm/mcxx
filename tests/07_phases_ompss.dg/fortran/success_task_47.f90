! <testinfo>
! test_generator=(config/mercurium-ompss "config/mercurium-ompss-2 openmp-compatibility")
! </testinfo>
SUBROUTINE FOO(M, N, V1, V2)
    IMPLICIT NONE
    INTEGER :: M, N
    INTEGER :: V1(M)
    INTEGER :: V2(M, N)
    LOGICAL :: ERR

    !$OMP TASK FIRSTPRIVATE(V1, V2) SHARED(ERR)
        ERR = ANY(V1 /= 42) .OR. ANY(V2 /= 23)
    !$OMP END TASK
    !$OMP TASKWAIT

    IF (ERR) STOP -1
END SUBROUTINE FOO

PROGRAM P
    IMPLICIT NONE
    INTEGER, PARAMETER :: M = 1
    INTEGER, PARAMETER :: N = 5
    INTEGER :: V1(M)
    INTEGER :: V2(M, N)
    V1 = 42
    V2 = 23

    CALL FOO(M, N, V1, V2)
END PROGRAM P
