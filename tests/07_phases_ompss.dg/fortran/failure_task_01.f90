! <testinfo>
! test_generator=(config/mercurium-ompss "config/mercurium-ompss-2 openmp-compatibility")
! test_compile_fail=yes
! </testinfo>
SUBROUTINE F(V, LEN)
    IMPLICIT NONE
    INTEGER ::V(*)
    iNTEGER :: LEN

    !$OMP TASK FIRSTPRIVATE(V)
        PRINT *, V(1)
    !$OMP END TASK
END SUBROUTINE F
