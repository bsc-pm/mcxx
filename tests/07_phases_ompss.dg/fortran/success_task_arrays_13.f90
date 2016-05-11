! <testinfo>
! test_generator="config/mercurium-ompss no-nanos6"
! </testinfo>
PROGRAM P
    IMPLICIT NONE
    INTEGER :: X(10, 20) 

    X = 1
    CALL FOO(X)
    !$OMP TASKWAIT

    IF (ANY(X(1, 1:10) /= 2)) STOP 1
END PROGRAM P

SUBROUTINE FOO(X)
IMPLICIT NONE
INTEGER :: X(10, *)

!$OMP TASK INOUT(X(1, 1:10))
   X(1, 1:10) = X(1, 1:10) + 1
!$OMP END TASK

END SUBROUTINE FOO
