! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>

!! This used to be a PROGRAM but passing internal procedures
!! is a Fortran 2008 feature
MODULE M
  CONTAINS

    SUBROUTINE S(F)
          REAL :: F
          PRINT *, F(1.0)
    END SUBROUTINE S

    FUNCTION R(X)
     REAL :: X, R
     R = X + 1.0
    END FUNCTION R

    SUBROUTINE S2
        CALL S(R) !! Failure
    END SUBROUTINE S2
END MODULE M
