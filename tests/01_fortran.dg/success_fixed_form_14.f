! <testinfo>
! test_generator="config/mercurium-fortran run"
! </testinfo>
C        1         2         3         4         5         6         7  |-------------
C2345678901234567890123456789012345678901234567890123456789012345678901234567890123456
      PROGRAMMAIN
      IMPLICITNONE
      CHARACTER(LEN=256) :: STR1, STR2
      INTEGER L
      STR1="*
     C*"
      L = LEN(TRIM(STR1))
      PRINT *, L
      IF (L /= 61) STOP 1
C        1         2         3         4         5         6         7  |-------------
C2345678901234567890123456789012345678901234567890123456789012345678901234567890123456
      STR2="*                                                                         
     C*"
      L = LEN(TRIM(STR2))
      PRINT *, L
      IF (L /= 61) STOP 2
      ENDPROGRAMMAIN
