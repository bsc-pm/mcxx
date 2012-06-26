! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
PROGRAM P
    INTRINSIC :: AINT
    INTRINSIC :: DINT

    PRINT *, AINT(A = 2.3_8, KIND = 8)
    PRINT *, DINT(A = 2.3_8)
END PROGRAM P
