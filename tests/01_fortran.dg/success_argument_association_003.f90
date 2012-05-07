! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
PROGRAM P
    INTERFACE
        ELEMENTAL SUBROUTINE S(A, B)
            INTEGER, INTENT(IN) :: A, B
        END SUBROUTINE S
    END INTERFACE 

    INTEGER :: X(10), Y(10)
    INTEGER :: SX, SY

    CALL S(SX, SY)
    CALL S(X, Y)

    CALL S(X, SY)
    CALL S(SX, Y)
END PROGRAM P
