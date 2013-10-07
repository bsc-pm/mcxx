! <testinfo>
! test_generator=config/mercurium-omp
! </testinfo>

MODULE M
CONTAINS
    FUNCTION FOO() RESULT (F1)
        REAL X, BETA, F1
        X = 1.0
        BETA = 1.0
        F1 = exp(-0.5*X*BETA)
        F1 = exp(-0.5*X*BETA)
    END FUNCTION FOO
END MODULE M


PROGRAM P
    USE M
    IMPLICIT NONE
    REAL :: RES

    RES = FOO()
END PROGRAM P
