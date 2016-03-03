! <testinfo>
! test_generator="config/mercurium-ompss"
! test_compile_fail_nanos6_mercurium=yes
! test_compile_fail_nanos6_imfc=yes
! </testinfo>
PROGRAM MAIN
    ! USE OMP_LIB
    IMPLICIT NONE
    INTEGER :: I, S0, S1(12), S2(3,4)

    S0 = 0
    S1 = 0
    S2 = 0

    !$OMP DO REDUCTION(+:S0) SCHEDULE(OMPSS_STATIC)
    DO I = 1, 100
      S0 = S0 + I
      ! PRINT "('THREAD=',I0,'  I=',I0,'  S0=',I0)", OMP_GET_THREAD_NUM(), I, S0
    END DO

    IF (S0 /= 5050) THEN
        PRINT *, "S0 is not 5050 but ", S0
        STOP 1
    END IF

    !$OMP DO REDUCTION(+:S1) SCHEDULE(OMPSS_STATIC)
    DO I = 1, 100
      S1 = S1 + I
    END DO

    IF (ANY(S1 /= 5050)) THEN
        STOP 2
    END IF

    !$OMP DO REDUCTION(+:S2) SCHEDULE(OMPSS_STATIC)
    DO I = 1, 100
      S2 = S2 + I
    END DO

    IF (ANY(S2 /= 5050)) THEN
        STOP 3
    END IF
END PROGRAM MAIN
