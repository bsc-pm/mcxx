! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
PROGRAM MAIN
    TYPE T
        INTEGER, POINTER :: A(:)
        CHARACTER(LEN=10), POINTER :: C
    END TYPE T

    TYPE(T):: X(10)
    INTEGER :: I

    IF (ASSOCIATED(X(I) % A)) THEN
        PRINT *, SIZE(X(I) % A)
        PRINT *, SIZE(X(I) % A, DIM=1)
        PRINT *, LBOUND(X(I) % A, DIM=1)
        PRINT *, UBOUND(X(I) % A, DIM=1)
        PRINT *, SHAPE(X(I) % A)
    END IF

END PROGRAM MAIN
