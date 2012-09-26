! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
PROGRAM P
    TYPE T
        INTEGER :: X, Y
    END TYPE T

    TYPE(T), SAVE :: A(10) = T(1, 2)

    PRINT *, A
END PROGRAM P
