! <testinfo>
! test_generator="config/mercurium-fortran run"
! </testinfo>
PROGRAM P
    IMPLICIT NONE
    INTEGER, PARAMETER :: N = 100
    INTEGER, POINTER :: X1
    INTEGER, TARGET :: V1

    INTEGER, POINTER :: X2(:)
    INTEGER, TARGET :: V2(N)

    INTEGER :: I

    X1 => V1
    X2(1:) => V2(1:)

    V1 = 0
    V2 = 0

    DO I=1, N
        X1 = X1 + 1
        X2(I) = X2(I) + 1
    ENDDO

    IF  (X1 /= N)  STOP 1
    IF  (ANY(X2 /= 1))  STOP 2

    NULLIFY(X1)
    NULLIFY(X2)
END PROGRAM P
