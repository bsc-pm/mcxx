! <testinfo>
! test_generator=config/mercurium-ompss
! </testinfo>
PROGRAM P
    IMPLICIT NONE
    INTEGER :: G, I
    INTEGER, PARAMETER :: MAX_GRAINSIZE = 20
    INTEGER, PARAMETER :: N = 2000
    INTEGER :: A(N)

    INTEGER :: L, U, S

    
    A = 0
    L = 1
    U = 2000
    S = 1
    DO G=1, MAX_GRAINSIZE

        !$OMP TASKLOOP SHARED(A) GRAINSIZE(G)
        DO I=L, U, S
            A(I) = A(I) + 1
        ENDDO
    ENDDO
    IF(ANY(A(::S) /= MAX_GRAINSIZE)) STOP -1


    A = 0
    L = 1
    U = 2000
    S = 3
    DO G=1, MAX_GRAINSIZE

        !$OMP TASKLOOP SHARED(A) GRAINSIZE(G)
        DO I=L, U, S
            A(I) = A(I) + 1
        ENDDO
    ENDDO
    IF(ANY(A(::S) /= MAX_GRAINSIZE)) STOP -2


    A = 0
    L = 2000
    U = 1
    S = -1
    DO G=1, MAX_GRAINSIZE

        !$OMP TASKLOOP SHARED(A) GRAINSIZE(G)
        DO I=L, U, S
            A(I) = A(I) + 1
        ENDDO
    ENDDO
    IF(ANY(A(::S) /= MAX_GRAINSIZE)) STOP -3


    A = 0
    L = 2000
    U = 1
    S = -3
    DO G=1, MAX_GRAINSIZE

        !$OMP TASKLOOP SHARED(A) GRAINSIZE(G)
        DO I=L, U, S
            A(I) = A(I) + 1
        ENDDO
    ENDDO
    IF(ANY(A(::S) /= MAX_GRAINSIZE)) STOP -4


    A = 0
    L = -999
    U = 1000
    S = 1
    DO G=1, MAX_GRAINSIZE

        !$OMP TASKLOOP SHARED(A) GRAINSIZE(G)
        DO I=L, U, S
            A(I+1000) = A(I+1000) + 1
        ENDDO
    ENDDO
    IF(ANY(A(::S) /= MAX_GRAINSIZE)) STOP -5


    A = 0
    L = -999
    U = 1000
    S = 3
    DO G=1, MAX_GRAINSIZE

        !$OMP TASKLOOP SHARED(A) GRAINSIZE(G)
        DO I=L, U, S
            A(I+1000) = A(I+1000) + 1
        ENDDO
    ENDDO
    IF(ANY(A(::S) /= MAX_GRAINSIZE)) STOP -6


    A = 0
    L = 999
    U = -1000
    S = -1
    DO G=1, MAX_GRAINSIZE

        !$OMP TASKLOOP SHARED(A) GRAINSIZE(G)
        DO I=L, U, S
            A(1000 - I) = A(1000 - I) + 1
        ENDDO
    ENDDO
    IF(ANY(A(::S) /= MAX_GRAINSIZE)) STOP -7


    A = 0
    L = 999
    U = -1000
    S = -3
    DO G=1, MAX_GRAINSIZE

        !$OMP TASKLOOP SHARED(A) GRAINSIZE(G)
        DO I=L, U, S
            A(1000 - I) = A(1000 - I) + 1
        ENDDO
    ENDDO
    IF(ANY(A(::S) /= MAX_GRAINSIZE)) STOP -8

END PROGRAM P
