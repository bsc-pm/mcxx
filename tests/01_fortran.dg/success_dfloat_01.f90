! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
PROGRAM P
    IMPLICIT NONE

    INTEGER(4) :: A1
    INTEGER(8) :: A2

    PRINT *, DFLOAT(A1)
    PRINT *, DFLOAT(A2)
END PROGRAM P
