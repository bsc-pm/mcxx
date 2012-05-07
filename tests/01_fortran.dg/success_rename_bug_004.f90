! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
SUBROUTINE S(A, B, N, V)
    IMPLICIT NONE
    INTEGER :: N, V
    INTEGER :: A(N)
    INTEGER :: B(N)

    A = V
    B = V
END SUBROUTINE S

PROGRAM P

    INTERFACE
        SUBROUTINE S(A, B, N, V)
            IMPLICIT NONE
            INTEGER :: N, V
            INTEGER :: A(N)
            INTEGER :: B(N)
        END SUBROUTINE S
    END INTERFACE

    INTEGER :: A(10)
    INTEGER :: B(10)

    A = 1
    B = 1

    PRINT *, A
    PRINT *, B
    PRINT *, "----"

    CALL S(A, B, 10, 12)

    PRINT *, A
    PRINT *, B
    PRINT *, "----"
END PROGRAM P
