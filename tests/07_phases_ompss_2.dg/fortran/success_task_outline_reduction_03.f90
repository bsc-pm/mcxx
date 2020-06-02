! <testinfo>
! test_generator=config/mercurium-ompss-2
! </testinfo>

! Reductions arrays outline 1

!$OSS TASK REDUCTION(+: P)
SUBROUTINE Q(P)
    IMPLICIT NONE
    INTEGER :: P

    P = P + 1

END SUBROUTINE Q

!$OSS TASK WEAKREDUCTION(+: P) FINAL(.TRUE.)
SUBROUTINE S(P)
    IMPLICIT NONE
    INTEGER :: P(10)

    CALL Q(P(1))
    CALL Q(P(2))
    CALL Q(P(3))
    CALL Q(P(4))
    CALL Q(P(5))
    CALL Q(P(6))
    CALL Q(P(7))
    CALL Q(P(8))
    CALL Q(P(9))
    CALL Q(P(10))

END SUBROUTINE S

PROGRAM MAIN
    IMPLICIT NONE

    INTEGER :: P(10)
    P = 0

    CALL S(P)

    !$OSS TASKWAIT

    PRINT *, P

    IF ( ANY(P /= 1) ) STOP 1

END PROGRAM MAIN
