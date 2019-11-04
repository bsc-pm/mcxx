! <testinfo>
! test_generator=config/mercurium-ompss-2
! test_nolink=yes
! </testinfo>
SUBROUTINE FOO(Y)
    IMPLICIT NONE
    INTEGER :: Y
END SUBROUTINE FOO

PROGRAM P
    IMPLICIT NONE
    INTEGER :: X
    INTERFACE
        !$OSS TASK VERIFIED INOUT(Y)
        SUBROUTINE FOO(Y)
            IMPLICIT NONE
            INTEGER :: Y
        END SUBROUTINE FOO
    END INTERFACE

    !$OSS TASK VERIFIED INOUT(X)
    !$OSS END TASK

    CALL FOO(X)

    !$OSS TASKWAIT
END PROGRAM P
