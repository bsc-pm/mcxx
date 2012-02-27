! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
PROGRAM P
  CONTAINS

    SUBROUTINE S(F)
          REAL :: F
          PRINT *, F(1.0)
    END

    FUNCTION R(X)
     REAL :: X, R
     R = X + 1.0
    END

    SUBROUTINE S2
        CALL S(R) !! Failure
    END
END
