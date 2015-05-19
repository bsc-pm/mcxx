! <testinfo>
! test_generator="config/mercurium-fortran run"
! </testinfo>
PROGRAM MAIN
    DIMENSION A(100)
    EQUIVALENCE (A(1), B(1))
    COMMON /FOO/ B(100)

    A(1) = 3
    IF (B(1) /= 3) STOP 1
END PROGRAM MAIN
