! <testinfo>
! test_generator=config/mercurium-ompss-2
! </testinfo>


! Reductions scalar

PROGRAM P
    INTEGER(4) :: X
    X = 0
    DO I=1,1000
        !$OSS TASK REDUCTION(+: X)
            X = X + 1;
        !$OSS END TASK
    END DO
    !$OSS TASKWAIT

    IF (X /= 1000) STOP 1

END PROGRAM P

