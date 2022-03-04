! <testinfo>
! test_generator=config/mercurium-ompss-2
! test_ignore=yes
! test_ignore_reason="feature not supported by nanos6"
! </testinfo>

SUBROUTINE BAR(X)
IMPLICIT NONE
INTEGER, INTENT(INOUT) :: X
X = 77
END SUBROUTINE

PROGRAM P
IMPLICIT NONE
INTEGER :: X
INTEGER :: I

X = 5

!$OSS TASK DO ONREADY(BAR(X)) SHARED(X)
DO I = 1, 10
CONTINUE
END DO
!$OSS END TASK DO

!$OSS TASKWAIT

IF (X /= 77) STOP 1

END PROGRAM
