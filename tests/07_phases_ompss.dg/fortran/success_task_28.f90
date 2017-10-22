! <testinfo>
! test_generator=(config/mercurium-ompss "config/mercurium-ompss-2 openmp-compatibility")
! test_nolink=yes
! </testinfo>

SUBROUTINE FOO(N)
    IMPLICIT NONE
    INTEGER :: N
    INTEGER :: A(N)
    INTEGER :: B(N)

    !$OMP TASK SHARED(A) INOUT(B)
    !$OMP END TASK

    !$OMP TASKWAIT
END SUBROUTINE FOO
