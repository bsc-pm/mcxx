! <testinfo>
! test_generator=config/mercurium-omp
! test_compile_fail=yes
! test_compile_end_signal=yes
! test_compile_faulty=yes
! </testinfo>
SUBROUTINE FOO(FUN, I)
    IMPLICIT NONE
    INTEGER :: I
    INTEGER :: V(10)
    INTERFACE
        SUBROUTINE FUN(N, V)
            INTEGER :: N
            INTEGER :: V(N+1)
        END SUBROUTINE FUN
    END INTERFACE

    !$OMP TASK
        CALL FUN(10, V)
    !$OMP END TASK
END SUBROUTINE FOO
