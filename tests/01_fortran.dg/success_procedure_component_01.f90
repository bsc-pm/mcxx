! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
MODULE M
    IMPLICIT NONE
    TYPE T
        PROCEDURE(), NOPASS, POINTER :: P
        PROCEDURE(INTEGER), NOPASS, POINTER :: P2
    END TYPE T
END MODULE M
