! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
SUBROUTINE S(F1, F2)
    INTERFACE
        FUNCTION F1(X)
             INTEGER :: X, F1
        END FUNCTION F1
    END INTERFACE
    OPTIONAL :: F1
    INTEGER, OPTIONAL, EXTERNAL :: F2
    INTEGER :: Y

    IF (PRESENT(F1)) THEN
     Y = F1(3)
    END IF

    IF (PRESENT(F2)) THEN
     Y = F2(3)
    END IF

END SUBROUTINE S
