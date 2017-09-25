! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
PROGRAM MAIN
    IMPLICIT NONE
    INTEGER  :: X, Y, Z
    TARGET :: X(10)
    TARGET :: Y(10), Z(20)

    X(1) = 2
    Y(3) = 4
    Z(5) = 6
END PROGRAM MAIN
