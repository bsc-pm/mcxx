! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
Subroutine BifG21 (W11, W20, NS)
    IMPLICIT NONE

    INTEGER :: NS
    REAL :: W11(NS)
    COMPLEX :: W20(NS)

    REAL :: Q1(NS)

    ! Q1(1:NS) = 2.D0 * W11(1:NS) + Real( W20(1:NS) )
    Q1 = W11(1:2) + 2
End
