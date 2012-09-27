! <testinfo>
! test_generator="config/mercurium-fortran run"
! </testinfo>
PROGRAM P
  IMPLICIT NONE

  CHARACTER(len=*) ,PARAMETER :: separator = REPEAT('-',79)
  INTEGER, PARAMETER :: S = LEN(separator)

  IF (S /= 79) THEN
      STOP 1
  END IF
END PROGRAM

