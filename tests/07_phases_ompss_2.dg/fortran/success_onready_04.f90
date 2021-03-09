! <testinfo>
! test_generator=config/mercurium-ompss-2
! test_compile_fail=yes
! </testinfo>

! FUNCTIONS are not supported

PROGRAM P
IMPLICIT NONE
INTEGER :: X
INTEGER :: I

X = 5

!$OSS TASK DO ONREADY(BAR(X))
DO I = 1, 10
CONTINUE
END DO
!$OSS END TASK DO

!$OSS TASKWAIT

IF (X /= 77) STOP 1

CONTAINS

FUNCTION BAR(I) RESULT(J)
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: I ! INPUT
  INTEGER             :: J ! OUTPUT
  J = I**2 + I**3
END FUNCTION

END PROGRAM
