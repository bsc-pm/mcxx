! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
PROGRAM P
    INTERFACE
        SUBROUTINE S(X, *, Y)
            INTEGER :: X, Y
        END SUBROUTINE S
    END INTERFACE

    CALL S(3, *100, 4)

    100 CONTINUE

END PROGRAM P
