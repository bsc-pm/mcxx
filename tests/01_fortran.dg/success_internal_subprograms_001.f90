! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
MODULE M
    CONTAINS

        SUBROUTINE S2(A, B)
            INTEGER :: A

            CALL S1(3, 4)
            CALL S1(B = 4)
        END SUBROUTINE S2

        SUBROUTINE S1(A, B)
            INTEGER, OPTIONAL :: A
            INTEGER :: B
        END SUBROUTINE S1
END MODULE M
