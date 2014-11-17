! <testinfo>
! test_generator="config/mercurium-fortran run"
! </testinfo>
PROGRAM MAIN
    DOUBLE PRECISION :: X
    PARAMETER ( X = 2.0 )

    DOUBLE PRECISION :: Y
    COMMON / C / Y

    Y = X

    CALL FOO(X)
END PROGRAM MAIN

SUBROUTINE FOO(X)
    DOUBLE PRECISION :: X

    DOUBLE PRECISION :: Y
    COMMON / C / Y

    IF (X /= Y) STOP 1
END SUBROUTINE FOO
