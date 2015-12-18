! <testinfo>
! test_generator=config/mercurium-ompss
! </testinfo>
PROGRAM P
    IMPLICIT NONE
    INTEGER, PARAMETER :: N = 100
    INTEGER, PARAMETER :: MAX_GRAINSIZE = 7
    INTEGER :: I, X
    INTEGER :: A(N)

    DO X=1, MAX_GRAINSIZE
        A = -1
        !$OMP TASKLOOP GRAINSIZE(X) OUT(A(I)) NOGROUP
        DO I=1, N
            A(I) = 0
        ENDDO

        !$OMP TASKLOOP GRAINSIZE(X) SHARED(A) INOUT(A(I))
        DO I=1, N
            IF (A(I) /= 0) STOP -1
            A(I) = A(I) + 1
        ENDDO
        IF (ANY(A /= 1)) STOP -2

    ENDDO
END PROGRAM P
