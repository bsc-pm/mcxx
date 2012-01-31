! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
SUBROUTINE S(B)
    INTEGER, POINTER :: B

    CALL S1(B)

    CONTAINS

        SUBROUTINE S1(A)
            INTEGER, POINTER :: A
        END
END 
