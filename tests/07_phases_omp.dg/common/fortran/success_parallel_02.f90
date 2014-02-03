! <testinfo>
! test_generator=config/mercurium-omp
! test_nolink=yes
! </testinfo>

MODULE M
    IMPLICIT NONE
    CONTAINS
        SUBROUTINE FOO(V, N)
            IMPLICIT NONE
            INTEGER :: N
            INTEGER :: V(N, *)
        END SUBROUTINE FOO

        SUBROUTINE BAR(V, N)
            IMPLICIT NONE
            INTEGER :: N
            INTEGER :: V(N, *)
            !$OMP PARALLEL
            CALL FOO(V, N)
            !$OMP END PARALLEL
        END SUBROUTINE BAR

END MODULE M


