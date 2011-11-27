! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
SUBROUTINE quux 
  IMPLICIT NONE
  INTEGER, PARAMETER :: M = 10
  INTEGER :: F, X, A

  F(X) = X + M + A

  PRINT *, F(3)

END SUBROUTINE quux
