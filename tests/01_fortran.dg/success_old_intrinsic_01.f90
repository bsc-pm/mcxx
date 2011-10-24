! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
PROGRAM P
    IMPLICIT NONE
    INTRINSIC DCONJG, DIMAG
    COMPLEX(KIND=8) :: K

    PRINT *, DCONJG(K)
    PRINT *, DIMAG(K)
END PROGRAM P
