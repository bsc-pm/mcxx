! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
SUBROUTINE S

    INTEGER, POINTER :: P

    LOGICAL :: L

    L = ASSOCIATED ( F() )

    CONTAINS
        FUNCTION F() RESULT(X)
            INTEGER, POINTER :: X 
            ALLOCATE(X)
        END FUNCTION F
END SUBROUTINE S
