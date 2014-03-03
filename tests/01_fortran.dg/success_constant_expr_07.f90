! <testinfo>
! test_ignore=yes
! </testinfo>
PROGRAM P
    TYPE T
        INTEGER, POINTER :: X
    END TYPE T

    ! This turned to be not valid Fortran
    TYPE(T), PARAMETER :: A = T(NULL())

    PRINT *, A % X
END PROGRAM P
