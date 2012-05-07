! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
PROGRAM P
    CHARACTER(LEN=1024), DIMENSION(:), POINTER :: X1

    CALL S(X1)

    CONTAINS
        SUBROUTINE S(X)
            CHARACTER(LEN=*), DIMENSION(:), POINTER :: X
        END SUBROUTINE S
END PROGRAM P
