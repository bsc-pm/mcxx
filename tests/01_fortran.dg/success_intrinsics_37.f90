! <testinfo>
! test_generator="config/mercurium-fortran run"
! </testinfo>
PROGRAM A
   IMPLICIT NONE
    REAL, PARAMETER :: VAL1 = -1.250
    REAL, PARAMETER :: VAL2 = 42.0
    INTEGER :: RES1  = NINT(VAL1)
    INTEGER :: RES2  = NINT(VAL2)

    REAL, PARAMETER :: VAL3(2) = (/ 1.2, -1.2 /)
    INTEGER, PARAMETER :: RES3(2) = NINT(VAL3)
    IF (RES1 /= -1) STOP 1
    IF (RES2 /= 42) STOP 1
    IF (RES3(1) /= 1) STOP 1
    IF (RES3(2) /= -1) STOP 1
END PROGRAM A
