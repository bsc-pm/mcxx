! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
PROGRAM FOO
    IMPLICIT NONE
        INTEGER :: Z(10)
        CALL BAZ(Z(3), 4)

    CONTAINS
    SUBROUTINE BAZ(X, N)
        IMPLICIT NONE
        INTEGER :: N
        INTEGER :: X(N, N)
        CONTINUE
    END SUBROUTINE BAZ

    SUBROUTINE QUUX(Y)
        IMPLICIT NONE
        INTEGER :: Y(10)
        !! This is valid since this a scalar bound to an array
        CALL BAZ(Y(3), 4)
    END SUBROUTINE QUUX
END PROGRAM FOO

