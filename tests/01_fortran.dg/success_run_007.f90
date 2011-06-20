! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
PROGRAM P
  CALL S()
  STOP 1
 CONTAINS

   SUBROUTINE S()
      STOP 0
   END SUBROUTINE S
END PROGRAM P
