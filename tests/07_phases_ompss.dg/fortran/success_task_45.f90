! <testinfo>
! test_generator=(config/mercurium-ompss "config/mercurium-ompss-2 openmp-compatibility")
! </testinfo>
PROGRAM P
    IMPLICIT NONE
    INTEGER, ALLOCATABLE :: X(:)
    INTEGER, PARAMETER :: N = 1000
    INTEGER :: RES, I


    ALLOCATE(X(N))

    X = 1
    rEs = 0

    DO I=1, N
        !$OMP TASK COMMUTATIVE(RES) IN(X(I))
            RES = RES + X(I)
        !$OMP END TASK
    ENDDO
    !$OMP TASKWAIT

    IF (RES /= N) STOP 1
END PROGRAM P
