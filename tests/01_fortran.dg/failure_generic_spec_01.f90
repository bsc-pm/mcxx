! <testinfo>
! test_generator=config/mercurium-fortran
! test_compile_fail=yes
! </testinfo>
PROGRAM P
    IMPLICIT NONE

    INTERFACE FOO
        SUBROUTINE F1(X)
             INTEGER :: X
        END SUBROUTINE F1
    END INTERFACE

    CALL FOO(1)
    !! FAILURE HERE
    CALL FOO(1.2)

    CONTAINS

        SUBROUTINE S
            INTERFACE FOO
                SUBROUTINE F2(X)
                     REAL :: X
                END SUBROUTINE F2
            END INTERFACE

            CALL FOO(1)
            CALL FOO(1.2)
        END SUBROUTINE S
END PROGRAM P
