! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
PROGRAM P

    INTEGER, ALLOCATABLE :: A(:)
    INTEGER, POINTER :: B(:)

    PRINT *, KIND(A)
    PRINT *, KIND(B)
END
