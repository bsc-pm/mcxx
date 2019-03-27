! <testinfo>
! test_generator=config/mercurium-ompss-2
! test_exec_fail=yes
! test_exec_faulty=yes
! </testinfo>
PROGRAM P
IMPLICIT NONE
INTEGER :: X

X = 1
!$OSS TASK INOUT(X)
    X = X + 1
    !$OSS RELEASE OUT(X)
    IF (X /= 2) STOP 1
!$OSS END TASK

!$OSS TASK IN(X)
    IF (X /= 2) STOP 1
!$OSS END TASK

!$OSS TASKWAIT
END PROGRAM P
