! <testinfo>
! test_generator=config/mercurium-ompss
! test_compile_fail_nanos6_mercurium=yes
! test_compile_fail_nanos6_imfc=yes
! </testinfo>
SUBROUTINE FOO(V, RES, N, M)
    IMPLICIT NONE
    INTEGER :: N, M
    INTEGER :: V(N, M)
    INTEGER, ALLOCATABLE :: RES(:)
    INTEGER :: I

    RES = 0
    DO I=1, M
        !$OMP TASK REDUCTION(+: RES) SHARED(V)
            !! PRINT *, "------> ", RES
            RES = RES + V(:, I)
        !$OMP END TASK
    ENDDO
    !$OMP TASKWAIT

END SUBROUTINE FOO


PROGRAM P
    IMPLICIT NONE

    INTERFACE
      SUBROUTINE FOO(V, RES, N, M)
        IMPLICIT NONE
        INTEGER :: N, M
        INTEGER :: V(N, M)
        INTEGER, ALLOCATABLE :: RES(:)
        INTEGER :: I
     END SUBROUTINE FOO
    END INTERFACE


    INTEGER, PARAMETER :: N = 10
    INTEGER, PARAMETER :: M = 10
    INTEGER :: V(N, M)
    INTEGER :: I,J
    INTEGER, ALLOCATABLE :: RES(:)

    ALLOCATE(RES(N))

    DO J=1,M
        DO I=1,N
            V(I, J) =  ((J-1)*M) + I
        ENDDO
    ENDDO

    CALL FOO(V, RES, N, M)

    !! PRINT *, SUM(RES), ((N*M*(N*M+1))/2)
    IF (SUM(RES) /= ((N*M*(N*M+1))/2))  STOP 1

    PRINT *, "ok!"
END PROGRAM P
