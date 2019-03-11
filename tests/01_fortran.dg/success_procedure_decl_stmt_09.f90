! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
MODULE M1
    PROCEDURE(), POINTER, SAVE, PUBLIC :: FOO_M1 => null()
    PROCEDURE(), PUBLIC :: BAR_M1
END MODULE M1

MODULE M2
    IMPLICIT NONE
    PROCEDURE(), POINTER, SAVE, PUBLIC :: FOO_M2 => null()
    PROCEDURE(), PUBLIC :: BAR_M2
END MODULE M2

PROGRAM P
    USE M1
    USE M2
    IMPLICIT NONE
    CALL FOO_M1()
    CALL FOO_M2()
    CALL BAR_M1()
    CALL BAR_M2()
END PROGRAM P
