! <testinfo>
! test_generator="config/mercurium-fortran run"
! </testinfo>
PROGRAM P
    IMPLICIT NONE
    INTEGER, PARAMETER :: N = 100

    TYPE T
    INTEGER, POINTER :: X1(:)
    END TYPE T

    INTEGER, TARGET :: V1(N)
    TYPE(T) :: VAR

    INTEGER :: I

    VAR % X1(1:) => V1(1:)
    V1 = 0

    DO I=1, N
        VAR % X1(I) = VAR % X1(I) + 1
    ENDDO

    IF (ANY(VAR % X1 /= 1)) STOP -1

    NULLIFY(VAR % X1)
END PROGRAM P
