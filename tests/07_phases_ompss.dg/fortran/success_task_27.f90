! <testinfo>
! test_generator=config/mercurium-ompss
! test_compile_fail_nanos6_mercurium=yes
! </testinfo>

PROGRAM P
    IMPLICIT NONE
    INTEGER :: I, LIMIT
        !$OMP TASK FINAL(.FALSE.)
        !$OMP END TASK

        !$OMP TASK FINAL(I < LIMIT)
        !$OMP END TASK

        !$OMP TASKWAIT
END PROGRAM P
