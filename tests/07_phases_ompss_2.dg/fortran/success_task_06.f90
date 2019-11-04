! <testinfo>
! test_generator=config/mercurium-ompss-2
! test_nolink=yes
! </testinfo>
PROGRAM P
    IMPLICIT NONE

    INTEGER :: X

    !$OSS TASK INOUT(X) VERIFIED
    !$OSS END TASK

    !$OSS TASK INOUT(X) VERIFIED(X < 5)
    !$OSS END TASK

    !$OSS TASK INOUT(X)
    !$OSS END TASK

    !$OSS TASKWAIT
END PROGRAM P
