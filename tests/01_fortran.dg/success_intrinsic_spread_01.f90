! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
PROGRAM MAIN
    IMPLICIT NONE
    LOGICAL, DIMENSION(10,10) :: GWORK6
    LOGICAL, DIMENSION(10) :: OTRIG
    INTEGER :: B

    GWORK6(:,:) = SPREAD( OTRIG(:), DIM=B, NCOPIES=B)

END PROGRAM MAIN
