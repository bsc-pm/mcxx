! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
MODULE M
    INTERFACE
        SUBROUTINE S(X)
            INTEGER :: X
        END SUBROUTINE S
    END INTERFACE

CONTAINS

    SUBROUTINE S1
        CALL S(X = 1)
    END SUBROUTINE S1

END MODULE M
