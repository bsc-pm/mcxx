! <testinfo>
! test_generator=config/mercurium-omp
! </testinfo>
PROGRAM MAIN
    IMPLICIT NONE

    INTEGER :: X
    INTEGER :: S
    NAMELIST /SIMULATION/ X

    S = 10
    OPEN (UNIT=S, STATUS="SCRATCH")
    X = 1234
    WRITE (UNIT=S, NML=SIMULATION)
    X = -1

!$OMP TASK
    CALL SUB()
!$OMP END TASK
!$OMP TASKWAIT

    IF (X /= 1234) THEN
        PRINT *, "X = ", X
    END IF
    CLOSE(UNIT=S)

    CONTAINS
        SUBROUTINE SUB()
            IMPLICIT NONE
            REWIND (UNIT=S)
            READ (UNIT=S, NML=SIMULATION)
        END SUBROUTINE SUB
END PROGRAM MAIN
