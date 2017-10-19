! <testinfo>
! test_generator=(config/mercurium-ompss "config/mercurium-ompss-2 openmp-compatibility")
! </testinfo>
PROGRAM P
    IMPLICIT NONE
    INTEGER, PARAMETER :: N = 100
    INTEGER, PARAMETER :: MAX_GRAINSIZE = 7
    INTEGER :: I, X
    INTEGER :: A(N)

    DO X=1, MAX_GRAINSIZE
        A = 0
        !$OMP TASKLOOP GRAINSIZE(X) SHARED(A)
        DO I=N, 1, -1
            A(I) = A(I) + 1
        ENDDO

        !$OMP TASKLOOP GRAINSIZE(X) SHARED(A)
        DO I=N, 1, -1
            A(I) = A(I) + 1
        ENDDO

        IF (ANY(A /= 2)) STOP -1
    ENDDO
END PROGRAM P
