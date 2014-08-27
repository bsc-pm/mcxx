! <testinfo>
! test_generator=config/mercurium-ompss
! </testinfo>

PROGRAM P
    IMPLICIT NONE

    !$OMP TASK
        !$OMP TASKYIELD
    !$OMP END TASK

    !$OMP TASKWAIT
END PROGRAM P
