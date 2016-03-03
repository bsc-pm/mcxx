! <testinfo>
! test_generator=config/mercurium-ompss
! test_compile_fail_nanos6_mercurium=yes
! test_compile_fail_nanos6_imfc=yes
! </testinfo>
FUNCTION OMP_IN_FINAL() RESULT(X)
    IMPLICIT NONE
    LOGICAL X
    STOP -1
    X = .FALSE.
END FUNCTION OMP_IN_FINAL

RECURSIVE FUNCTION F(N) RESULT(RES)
    IMPLICIT NONE
    INTEGER :: N, RES
    LOGICAL :: X
    LOGICAL, EXTERNAL  :: OMP_IN_FINAL
    RES = 0
    IF (N > 0) THEN
    !$OMP TASK SHARED(RES)
        RES = F(N-1)
        IF(OMP_IN_FINAL()) THEN
            RES = RES + 1
        ELSE
            RES = RES + 2
        ENDIF
    !$OMP END TASK
    ENDIF
END FUNCTION F

PROGRAM P
    IMPLICIT NONE
    INTEGER :: RES
    INTEGER, EXTERNAL :: F
    RES = 0

    !$OMP TASK SHARED(RES) FINAL(.TRUE.)
        RES = F(10)
    !$OMP END TASK
    !$OMP TASKWAIT

    IF (RES /= 10) STOP -2
END PROGRAM P
