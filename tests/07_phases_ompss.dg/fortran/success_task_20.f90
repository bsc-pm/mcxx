! <testinfo>
! test_generator=config/mercurium-ompss
! test_compile_fail_nanos6_mercurium=yes
! test_compile_fail_nanos6_imfc=yes
! </testinfo>
SUBROUTINE FOO(X)
    INTEGER :: X
    X = X + 1
END SUBROUTINE FOO

PROGRAM P
    IMPLICIT NONE

    INTERFACE
        !$OMP TASK INOUT(X) FINAL(1)
        SUBROUTINE FOO(X)
            INTEGER :: X
        END SUBROUTINE FOO
    END INTERFACE
    INTEGER X

    X = -1

     CALL FOO(X)
!$OMP TASKWAIT ON(X)
IF (X /= 0) THEN
    STOP -1
END IF

    !$OMP TASK INOUT(X) FINAL(1)
         X = X + 1
     !$OMP END TASK
!$OMP TASKWAIT ON(X)

IF (X /= 1) THEN
    STOP -1
END IF

END PROGRAM P
