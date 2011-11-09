! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
PROGRAM P    
      IMPLICIT CHARACTER*14 (C)
      CHARACTER *10 CURL

      CARL = "A"
      CURL = "A"

      IF (CARL /= CURL) THEN
        STOP "WRONG"
      END IF
END PROGRAM P

