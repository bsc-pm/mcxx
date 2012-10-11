! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
MODULE M
    PRIVATE

    INTERFACE FOO
        MODULE PROCEDURE FOO
    END INTERFACE FOO

    CONTAINS

        SUBROUTINE FOO(X)
            INTEGER :: X
        END SUBROUTINE FOO
END MODULE M
