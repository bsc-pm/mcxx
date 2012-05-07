! <testinfo>
! test_generator="config/mercurium-fortran run"
! </testinfo>
PROGRAM P
    IMPLICIT NONE
    CHARACTER(LEN=256), PARAMETER :: A = "HOLA MON"

    PRINT *, LEN(A)
    IF (LEN(A) /= 256) STOP "ERROR"
END PROGRAM P
