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
        A = 0
        !$OMP TASKLOOP GRAINSIZE(X) SHARED(A)
        DO I=1, N
            A(I) = A(I) + 1
        ENDDO

        !$OMP TASKLOOP GRAINSIZE(X) SHARED(A)
        DO I=1, N
            A(I) = A(I) + 1
        ENDDO

        IF (ANY(A /= 2)) STOP -1
    ENDDO
END PROGRAM P
