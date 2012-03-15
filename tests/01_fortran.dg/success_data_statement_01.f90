! <testinfo>
! test_generator="config/mercurium-fortran run"
! </testinfo>
BLOCK DATA
    INTEGER :: X
    COMMON /FOO/ X
    DATA X / 1234 /
END BLOCK DATA

PROGRAM P
    IMPLICIT NONE
    INTEGER :: X
    COMMON /FOO/ X

    IF (X /= 1234) THEN
        STOP "WRONG"
    END IF
END PROGRAM P


