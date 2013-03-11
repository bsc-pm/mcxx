! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
MODULE M1

                IMPLICIT NONE

PRIVATE

PUBLIC :: FOO

INTERFACE FOO
        MODULE PROCEDURE FOOBAR
        MODULE PROCEDURE FOO
END INTERFACE FOO

CONTAINS
        FUNCTION FOOBAR(X)
                INTEGER :: X, FOOBAR
                FOOBAR = 0
        END FUNCTION FOOBAR

        FUNCTION FOO(X)
                REAL :: X, FOO
                FOO = 0.0
        END FUNCTION FOO

END MODULE M1

MODULE M2
   USE M1
                IMPLICIT NONE

 CONTAINS

END MODULE M2

MODULE M3
        USE M2
                IMPLICIT NONE

CONTAINS

        SUBROUTINE JARL
                REAL :: R
                INTEGER :: I

                R = FOO(R)
                I = FOO(I)

        END SUBROUTINE JARL


END MODULE M3
