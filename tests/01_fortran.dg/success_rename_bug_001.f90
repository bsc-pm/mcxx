! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
SUBROUTINE S
    INTEGER :: A
    INTERFACE
        SUBROUTINE T(A)
            INTEGER :: A
        END SUBROUTINE T
    END INTERFACE

    A = 1
    CALL T(2)
END SUBROUTINE S
