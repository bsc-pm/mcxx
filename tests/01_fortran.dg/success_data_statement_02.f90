! <testinfo>
! test_generator="config/mercurium-fortran run"
! </testinfo>

PROGRAM P
    IMPLICIT NONE
    INTEGER :: X
    COMMON /FOO/ X
    DATA X / 1234 /

    IF (X /= 1234) THEN
        STOP "WRONG"
    END IF
END PROGRAM P


