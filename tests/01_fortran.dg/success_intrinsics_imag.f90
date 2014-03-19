! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
PROGRAM MAIN
    IMPLICIT NONE

    COMPLEX(KIND=4) :: C4
    COMPLEX(KIND=4) :: C8

    C4 = (1.0_4, 0.0_4)
    C8 = (2.0_8, 0.0_8)

    PRINT *, IMAG(C4)
    PRINT *, IMAGPART(C4)
    PRINT *, IMAG(C8)
    PRINT *, IMAGPART(C8)

END PROGRAM MAIN
