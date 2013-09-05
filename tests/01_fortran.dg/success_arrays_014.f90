! <testinfo>
! test_generator="config/mercurium-fortran run"
! </testinfo>
PROGRAM MAIN
    IMPLICIT NONE

    INTEGER :: I
    INTEGER, PARAMETER :: N = 64
    INTEGER, PARAMETER :: A(N, N, N) = 12

    INTEGER, PARAMETER :: M = 32
    INTEGER, PARAMETER :: B(M * M * M) = (/ (I, I=1, M*M*M) /)

    INTEGER :: X

    IF (A(1, 2, 3) /= 12) STOP 1
    IF (A(2, 5, 6) /= 12) STOP 2

    IF (B(99) /= 99) STOP 3
    IF (B(128) /= 128) STOP 4

    ! Force constants be emitted
    X = A(1, 2, 3)
    IF (X /= 12) STOP 5

    X = B(33)
    IF (X /= 33) STOP 6
END PROGRAM MAIN
