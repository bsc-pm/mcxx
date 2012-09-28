! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
PROGRAM P
    TYPE T
        INTEGER, POINTER :: X
    END TYPE T

    TYPE(T), PARAMETER :: A = T(NULL())

    PRINT *, A % X
END PROGRAM P
