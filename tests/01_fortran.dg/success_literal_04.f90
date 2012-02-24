! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
PROGRAM P
    IMPLICIT NONE
    INTEGER, PARAMETER :: X = -1

    REAL(KIND=4), PARAMETER :: R4 = -1
    REAL(KIND=8), PARAMETER :: R8 = -1
    ! REAL(KIND=16), PARAMETER :: R16 = -1

    INTEGER(KIND=4) :: N4

    REAL(KIND=4) :: S4
    REAL(KIND=8) :: S8
    ! REAL(KIND=16) :: S16

    N4 = 0
    N4 = N4 + X

    S4 = S4 + R4
    S8 = S8 + R8
    ! S16 = S16 + R16
END PROGRAM P
