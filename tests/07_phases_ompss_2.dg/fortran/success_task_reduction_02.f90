! <testinfo>
! test_generator=config/mercurium-ompss-2
! test_compile_fail_nanos6_mercurium=yes
! test_compile_fail_nanos6_imfc=yes
! </testinfo>

! Reductions arrays 1

PROGRAM P
    INTEGER :: X(100)
    DO I=1,100
        X(I) = 0
    END DO
    DO I=1,100
        !$OSS TASK REDUCTION(+: X)
            X(I) = X(I) + 1;
        !$OSS END TASK
    END DO
    !$OSS TASKWAIT

    IF (ANY(X /= 1)) STOP 1

END PROGRAM P

