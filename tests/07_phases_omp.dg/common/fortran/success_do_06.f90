! <testinfo>
! test_generator=config/mercurium-omp
! </testinfo>
PROGRAM P
IMPLICIT NONE
INTEGER :: I, J, K

!$OMP DO
DO I = 1, 100
    DO J = 1, 100
    EXIT
    END DO
END DO
END PROGRAM P
