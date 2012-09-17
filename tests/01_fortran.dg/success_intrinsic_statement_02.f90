! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
MODULE MOO
   INTRINSIC :: EXP

CONTAINS
  SUBROUTINE S(X)
    INTRINSIC :: EXP
    PRINT *, EXP(1.2)
  END SUBROUTINE S
END MODULE MOO

