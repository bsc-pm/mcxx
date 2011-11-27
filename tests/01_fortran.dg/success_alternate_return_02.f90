! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
SUBROUTINE S(X, *, Y)
    INTEGER :: X, Y

    X = X + Y

    RETURN 1
END SUBROUTINE S
