! <testinfo>
! test_generator=config/mercurium-ompss-2
! </testinfo>
PROGRAM P
    IMPLICIT NONE
    INTEGER, POINTER :: X
    INTEGER, ALLOCATABLE :: Y

    ALLOCATE(X)
    ALLOCATE(Y)

    !$OSS TASK OUT(X, Y)
        X = 4
        Y = 2
    !$OSS END TASK

    !$OSS TASK IN(X, Y)
        IF (X /= 4) STOP 1
        IF (Y /= 2) STOP 2
    !$OSS END TASK

    !$OSS TASKWAIT

    DEALLOCATE(X)
    DEALLOCATE(Y)
END PROGRAM P
