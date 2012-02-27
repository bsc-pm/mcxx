! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
PROGRAM P

    IMPLICIT NONE
    INTEGER :: M(10, 20)

    INTEGER :: X, I, J

    DO I = 1, 10
     DO J = 1, 20
        M(J, I) = I + J
     END DO
    END DO

    X = MAXVAL(M(:, :))
    X = MAXVAL(M)

    X = MINVAL(M(:, :))
    X = MINVAL(M)
END PROGRAM P
