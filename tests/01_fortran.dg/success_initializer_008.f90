! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
MODULE M
    IMPLICIT NONE

    TYPE T
        REAL :: R
    END TYPE T

    TYPE(T) :: X2(0:1) = T(1.0) ! KO
END MODULE M
