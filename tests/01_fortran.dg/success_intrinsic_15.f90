! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
PROGRAM P
    INTEGER :: X(2)

    INTEGER :: M(2, 3)

    X = (/ 2, 3 /)
    M = RESHAPE((/ 1, 2, 3, 4, 5, 6 /), X)
END


