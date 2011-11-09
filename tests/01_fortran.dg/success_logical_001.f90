! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
PROGRAM P
    IMPLICIT NONE
    LOGICAL :: L1, L2

    L2 = .FALSE.
    L1 = L2

    IF (L1 .EQV. L2) STOP 1
    IF (L1 .NEQV. L2) STOP 1
END PROGRAM P
