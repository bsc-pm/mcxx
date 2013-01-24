! <testinfo>
! test_generator="config/mercurium-fortran run"
! </testinfo>
PROGRAM main
IMPLICIT NONE
  INTEGER, PARAMETER :: char_normal = 256

  TYPE, PUBLIC                          :: LocaleType
    CHARACTER(LEN=char_normal)          :: locale(5) = ""
  END TYPE LocaleType

  TYPE(LocaleType) :: A

  IF (SIZE(A%LOCALE) /= 5) STOP 1
END PROGRAM main
