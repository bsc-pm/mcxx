! <testinfo>
! test_generator=config/mercurium-ompss
! </testinfo>
PROGRAM MAIN
    IMPLICIT NONE
    INTEGER :: LOCAL

    LOCAL = 1
    CALL FOO(1)
    !$OMP TASKWAIT

    LOCAL = 2
    CALL FOO(2)
    !$OMP TASKWAIT
CONTAINS
    !$OMP TASK
    SUBROUTINE FOO(CHECK)
        IMPLICIT NONE
        INTEGER, VALUE :: CHECK

        IF (CHECK /= LOCAL) STOP 1
    END SUBROUTINE
END PROGRAM MAIN
