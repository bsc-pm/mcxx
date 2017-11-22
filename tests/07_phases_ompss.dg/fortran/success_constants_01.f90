! <testinfo>
! test_generator=(config/mercurium-ompss "config/mercurium-ompss-2 openmp-compatibility")
! test_nolink=yes
! </testinfo>

!! Despite the fact that this test is testing a feature of our Fortran FE, it's
!! here because we want to test it with different backend compilers
MODULE M
    IMPLICIT NONE

    CHARACTER(50), PARAMETER :: STR = "OMG"
    INTEGER, PARAMETER ::  V(3) = (/ 42, 42, 42/)

    CONTAINS

    SUBROUTINE FOO(S)
        IMPLICIT NONE
        CHARACTER(50) :: S
        PRINT *, S
    END SUBROUTINE FOO

    SUBROUTINE BAR()
        IMPLICIT NONE
        PRINT *, V
        PRINT *, STR
        CALL FOO(STR)
    END SUBROUTINE BAR
END MODULE M

PROGRAM P
    USE M
    IMPLICIT NONE
    CALL BAR()
END PROGRAM P
