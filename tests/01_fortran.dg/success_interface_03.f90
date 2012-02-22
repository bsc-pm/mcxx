! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
MODULE M
    PRIVATE
    PUBLIC :: FOO

    INTERFACE FOO
        MODULE PROCEDURE QUUX
        MODULE PROCEDURE BAR
    END INTERFACE FOO
    CONTAINS

        SUBROUTINE QUUX(A)
            REAL :: A
        END 

        SUBROUTINE BAR(N)
            INTEGER :: N
        END 
END

PROGRAM P
    USE M

    CALL FOO(3)
    CALL FOO(3.4)
END PROGRAM
