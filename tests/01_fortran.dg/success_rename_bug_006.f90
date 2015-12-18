! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
MODULE MOO
    IMPLICIT NONE
    CONTAINS
        SUBROUTINE MY_MOO(Z, FUN)
            INTERFACE
                FUNCTION FUN(X)
                    REAL(8) :: FUN
                    REAL(8) :: X
                END FUNCTION FUN
            END INTERFACE
            REAL(8) :: Z, T

            T = FUN(Z)
        END SUBROUTINE MY_MOO
END MODULE MOO

SUBROUTINE SUB(X)
    USE MOO, ONLY: MY_MOO
    IMPLICIT NONE
    REAL(8) :: T, X

    PRINT *, "HELLO"
    CALL MY_MOO(X, FOO)

    CONTAINS
        FUNCTION FOO(Y)
            IMPLICIT NONE
            REAL(8) :: FOO
            REAL(8) :: Y
            REAL(8) :: X
            X = 1.2_8
            FOO = 3.4_8 + X
        END FUNCTION FOO
END SUBROUTINE SUB
