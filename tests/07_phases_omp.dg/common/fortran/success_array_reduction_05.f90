! <testinfo>
! test_generator=config/mercurium-omp
! </testinfo>
SUBROUTINE foo(A, N)
    IMPLICIT NONE
    INTEGER :: N
    INTEGER, DIMENSION(N) :: A
    !$OMP PARALLEL REDUCTION(+:a)
      A = 1
    !$OMP END PARALLEL
END SUBROUTINE foo

PROGRAM MAIN
    IMPLICIT NONE
    INTEGER :: A(20)
    INTEGER :: T
    INTEGER, EXTERNAL :: OMP_GET_MAX_THREADS

    T = OMP_GET_MAX_THREADS()

    A = 0

    CALL FOO(A, 20)

    IF (ANY(A /= T)) STOP 1
END PROGRAM MAIN
