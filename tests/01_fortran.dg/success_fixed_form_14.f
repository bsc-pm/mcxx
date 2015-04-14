! <testinfo>
! test_generator="config/mercurium-fortran run"
! </testinfo>
C        1         2         3         4         5         6         7  |-------------
C2345678901234567890123456789012345678901234567890123456789012345678901234567890123456
      PROGRAMMAIN
      IMPLICITNONE
      CHARACTER(LEN=256) :: STR
      INTEGER L
      STR ="*
     C*"
      L = LEN(TRIM(STR))
      PRINT *, L
      IF (L /= 61) STOP 1
      ENDPROGRAMMAIN
