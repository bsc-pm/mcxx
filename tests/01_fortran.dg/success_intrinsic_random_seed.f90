! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
PROGRAM P
    IMPLICIT NONE

    INTEGER :: SIZE, SEED, I

    call random_seed(PUT  = (/(seed,i=1,size)/))
END PROGRAM P
