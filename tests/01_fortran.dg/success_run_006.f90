! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
PROGRAM P
   IMPLICIT NONE

   INTEGER :: A, B

   EQUIVALENCE (A, B)

   INTEGER :: VA(10), VB(10)

   EQUIVALENCE (VA(1), VB(2))

   A = 3
   IF (B /= 3) STOP 1

   VA(1) = 4
   IF (VB(2) /= 4) STOP 1
END PROGRAM P
