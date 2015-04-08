! <testinfo>
! test_generator=config/mercurium-ompss
! </testinfo>

PROGRAM MAIN
    IMPLICIT NONE
    INTEGER, TARGET :: T
    INTEGER, POINTER :: P
    !$OMP TASK
    P => T
    !$OMP END TASK

    !$OMP TASKWAIT
END PROGRAM MAIN
