! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
PROGRAM P
    IMPLICIT NONE
    DIMENSION :: C(6, 10)
    CHARACTER :: C * 10

    PRINT *, C(1, 2)(3:4)
END PROGRAM P
