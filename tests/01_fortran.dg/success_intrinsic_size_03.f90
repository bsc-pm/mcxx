! <testinfo>
! test_generator="config/mercurium-fortran run"
! </testinfo>
SUBROUTINE FOO(X, Y)
    INTEGER(4) :: X , Y
    IF (X /= 10) STOP -1
    IF (Y /= 20) STOP -2
END SUBROUTINE FOO


PROGRAM P
    IMPLICIT NONE
    INTEGER :: X(10, 20)
    CALL FOO(SIZE(X, 1), SIZE(X, 2))
END PROGRAM  P
