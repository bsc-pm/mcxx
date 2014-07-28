! <testinfo>
! test_generator="config/mercurium-fortran run"
! </testinfo>
PROGRAM P
    IMPLICIT NONE
    INTEGER :: X(10)
    CALL FOO(SIZEOF(X))
    CONTAINS

        SUBROUTINE FOO(X)
            USE ISO_C_BINDING, ONLY: C_SIZE_T
            INTEGER(C_SIZE_T) :: X
        END SUBROUTINE FOO
END PROGRAM P
