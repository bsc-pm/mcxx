! <testinfo>
! test_generator="config/mercurium-fortran"
! </testinfo>
SUBROUTINE S(A, B, X, Y)
    IMPLICIT NONE
    INTEGER :: A, B

    INTEGER :: X(A*2, B*2)
    INTEGER :: Y(:)

    INTEGER :: V(SIZE(X, DIM=1))
    INTEGER :: W(A*3)

    PRINT *, V
    PRINT *, W
    PRINT *, SIZE(X, DIM=1)
    PRINT *, SIZE(Y, DIM=1)
END SUBROUTINE S
