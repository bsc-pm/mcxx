! <testinfo>
! test_generator=config/mercurium-ompss
! </testinfo>

PROGRAM P
    IMPLICIT NONE
    REAL(4) :: V(10)
    REAL(4) :: RES, RES2
    INTEGER :: I

    RES = 100

    DO I= I, 10
        V(I) = I
    END DO

    !$OMP DO REDUCTION(MIN: RES)
    DO I=1, 10
        IF (RES > V(I)) RES = V(I)
    END DO

    IF  (RES /= 1.0) STOP 1
END PROGRAM P

