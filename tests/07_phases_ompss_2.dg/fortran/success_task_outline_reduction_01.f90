! <testinfo>
! test_generator=config/mercurium-ompss-2
! </testinfo>

! Reductions arrays outline 1

!$OSS TASK REDUCTION(+: P)
SUBROUTINE S(P, I)
    IMPLICIT NONE
    INTEGER :: P(10)
    INTEGER :: I

    P(I) = P(I) + 1

END SUBROUTINE S

PROGRAM MAIN
    IMPLICIT NONE

    INTEGER :: P(10)
    P = 0
    CALL S(P, 1)
    CALL S(P, 2)
    CALL S(P, 3)
    CALL S(P, 4)
    CALL S(P, 5)
    CALL S(P, 6)
    CALL S(P, 7)
    CALL S(P, 8)
    CALL S(P, 9)
    CALL S(P, 10)
    !$OSS TASKWAIT

    IF ( ANY(P /= 1) ) STOP 1

END PROGRAM MAIN
