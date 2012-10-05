! <testinfo>
! test_generator="config/mercurium-fortran run"
! </testinfo>
PROGRAM MAIN
    IMPLICIT NONE

    TYPE T
        INTEGER :: X, Y
    END TYPE T

    TYPE(T), PARAMETER, DIMENSION(2) :: A1= (/ T(1, 2), T(3, 4) /)
    TYPE(T), PARAMETER, DIMENSION(3) :: A2= (/ T(5, 6), T(7, 8), T(9, 10) /)

    TYPE(T), PARAMETER, DIMENSION(1:5) :: A3_1_5 = (/ A1, A2 /)
    TYPE(T), PARAMETER, DIMENSION(0:4) :: A3_0_4 = (/ A1, A2 /)

    LOGICAL, PARAMETER :: L = A3_1_5(2) % X == A3_0_4(1) % X

    IF (.NOT. L) STOP 1

END PROGRAM MAIN
