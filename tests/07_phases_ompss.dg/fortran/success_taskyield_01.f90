! <testinfo>
! test_generator=config/mercurium-ompss
! test_compile_fail_nanos6_mercurium=yes
! </testinfo>

PROGRAM P
    IMPLICIT NONE

    !$OMP TASK
        !$OMP TASKYIELD
    !$OMP END TASK

    !$OMP TASKWAIT
END PROGRAM P
