! <testinfo>
! test_generator="config/mercurium-fortran run"
! </testinfo>
PROGRAM P
USE ISO_C_BINDING
IMPLICIT NONE
    INTERFACE
        SUBROUTINE FOO(X)
            IMPORT :: C_FLOAT
            IMPLICIT NONE
            REAL(C_FLOAT) :: X
        END SUBROUTINE FOO
    END INTERFACE
    CALL FOO(1.0)
END PROGRAM P

SUBROUTINE FOO(X)
    USE ISO_C_BINDING
    IMPLICIT NONE
    REAL(C_FLOAT) :: X
END SUBROUTINE FOO
