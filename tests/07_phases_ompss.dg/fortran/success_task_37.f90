! <testinfo>
! test_generator=config/mercurium-ompss
! test_compile_fail_nanos6_mercurium=yes
! test_compile_fail_nanos6_imfc=yes
! </testinfo>
SUBROUTINE S1(P, S)
    IMPLICIT NONE
    INTEGER, EXTERNAL :: OMP_GET_THREAD_NUM, OMP_GET_MAX_THREADS
    INTEGER :: S
    EXTERNAL :: P

    !$OMP TASK SHARED(S)
    CALL S2(P, OMP_GET_THREAD_NUM(), S)
    !$OMP END TASK
END SUBROUTINE S1

SUBROUTINE S2(P, I, S)
    EXTERNAL :: P
    INTEGER :: S

    CALL P(I, S)
END SUBROUTINE S2

SUBROUTINE S3(X, S)
    INTEGER :: X, S

    PRINT *, "X = ", X

    !$OMP ATOMIC
    S = S + 1
END SUBROUTINE S3

PROGRAM MAIN
    EXTERNAL :: S3
    INTEGER :: S, I
    INTEGER, EXTERNAL :: OMP_GET_MAX_THREADS

    S = 0

    DO I = 1, OMP_GET_MAX_THREADS()
        CALL S1(S3, S)
    END DO
    !$OMP TASKWAIT
    PRINT *, S, "=?=", OMP_GET_MAX_THREADS()
    IF (S /= OMP_GET_MAX_THREADS()) STOP 1
END PROGRAM MAIN
