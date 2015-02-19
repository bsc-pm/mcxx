! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
SUBROUTINE SUB(X)
    INTERFACE
        SUBROUTINE FOO(A, B)
            INTEGER :: A
            REAL :: B
        END SUBROUTINE FOO

        SUBROUTINE BAR(A, B)
            INTEGER :: A
            REAL :: B
        END SUBROUTINE BAR
    END INTERFACE

    PROCEDURE(FOO) :: X, Y


    CALL FOO(A = 3, B = 4.0)
    CALL X(A = 3, B = 4.0)
    CALL Y(A = 3, B = 4.0)

    CALL FOO(3, 4.0)
    CALL X(3, 4.0)
    CALL Y(3, 4.0)

    CALL FOO(B = 4.0, A = 3)
    CALL X(B = 4.0, A = 3)
    CALL Y(B = 4.0, A = 3)
END SUBROUTINE SUB

