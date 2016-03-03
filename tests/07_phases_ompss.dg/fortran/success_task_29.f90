! <testinfo>
! test_generator=config/mercurium-ompss
! test_compile_fail_nanos6_mercurium=yes
! test_compile_fail_nanos6_imfc=yes
! </testinfo>
PROGRAM P
    IMPLICIT NONE
    INTEGER, PARAMETER :: N = 1000
    INTEGER ::  X1, X2

    X1 = -1
    X2 = -1
    !$OMP TASK SHARED(X1, X2)
        !$OMP DO LASTPRIVATE(X1)
        DO X1=0, N-1
        ENDDO

        !$OMP DO
        DO X2=0, N-1
        ENDDO
    !$OMP END TASK
    !$OMP TASKWAIT

    IF (X1 /= 1000) STOP 1
    IF (X2 /= -1) STOP 2

END PROGRAM P
