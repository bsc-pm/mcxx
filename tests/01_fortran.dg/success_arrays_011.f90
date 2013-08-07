! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
MODULE M
    IMPLICIT NONE
    INTEGER :: A(0,2) = 0
    INTEGER :: B(2,0) = 0
END MODULE M
