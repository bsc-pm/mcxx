! <testinfo>
! test_generator=config/mercurium-omp
! </testinfo>
PROGRAM P
    IMPLICIT NONE
    INTEGER :: A(10)
    !$OMP ATOMIC
    A = A + 1
END PROGRAM P

