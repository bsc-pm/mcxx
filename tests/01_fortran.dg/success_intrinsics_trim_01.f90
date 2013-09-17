! <testinfo>
! test_generator="config/mercurium-fortran run"
! </testinfo>
PROGRAM MAIN
   IMPLICIT NONE
   CHARACTER(len=10), PARAMETER :: orig = '   12345  '
   CHARACTER(len=*),  PARAMETER :: trimmed = trim(orig)

   IF (LEN(orig) /= 10) STOP 1
   IF (LEN(trimmed) /= 8) STOP 2
END PROGRAM MAIN
