! <testinfo>
! test_generator=config/mercurium-omp
! </testinfo>
MODULE MOO
    ! --
CONTAINS
    SUBROUTINE foo(A)
        IMPLICIT NONE
        INTEGER, DIMENSION(:) :: A
        !$OMP PARALLEL REDUCTION(+:a)
        A = 1
        !$OMP END PARALLEL
    END SUBROUTINE foo
END MODULE MOO


PROGRAM MAIN
    USE MOO
    IMPLICIT NONE
    INTEGER :: A(100)
    INTEGER :: T
    INTEGER, EXTERNAL :: OMP_GET_MAX_THREADS

    T = OMP_GET_MAX_THREADS()

    A = 0

    CALL FOO(A)

    IF (ANY(A /= T)) STOP 1
END PROGRAM MAIN
