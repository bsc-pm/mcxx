! <testinfo>
! test_generator=config/mercurium-omp
! </testinfo>
MODULE M1
    INTEGER :: N
END MODULE M1

SUBROUTINE S(V)
    USE M1, ONLY: N
    IMPLICIT NONE
    INTEGER :: V(N, N)
    INTEGER :: I

    !$OMP PARALLEL DO PRIVATE(V)
    DO I = 1, 10
        V(1:N, 1:N) = TRANSPOSE ( V(1:N, 1:N) )
    END DO
END SUBROUTINE S

PROGRAM P
    USE M1, ONLY: N
    IMPLICIT NONE
    INTEGER :: A(10, 10)

    N = 10
    CALL S(A)
END PROGRAM P

