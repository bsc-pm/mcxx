! <testinfo>
! test_generator=config/mercurium-fortran
! compile_versions="mod use all"
! test_FFLAGS_mod="-DWRITE_MOD"
! test_FFLAGS_use="-DUSE_MOD"
! test_FFLAGS_all="-DWRITE_MOD -DUSE_MOD"
! </testinfo>

#ifdef WRITE_MOD
MODULE MOO
    IMPLICIT NONE

    TYPE T
        PROCEDURE(IFOO1), NOPASS, POINTER :: FOO1
        PROCEDURE(IFOO2), PASS,   POINTER :: FOO2
    END TYPE T

    INTERFACE
        SUBROUTINE IFOO1(X)
            IMPLICIT NONE
            INTEGER :: X
        END SUBROUTINE IFOO1

        SUBROUTINE IFOO2(X)
            IMPORT T
            IMPLICIT NONE
            CLASS(T) :: X
        END SUBROUTINE IFOO2
    END INTERFACE
END MODULE MOO
#endif


#ifdef USE_MOD
PROGRAM P
    USE MOO
    IMPLICIT NONE

    TYPE(T) :: VAR
        VAR % FOO1 => SUB_PASS1
        VAR % FOO2 => SUB_PASS2

        CALL VAR % FOO1(1)
        CALL VAR % FOO2()

    CONTAINS
    
        SUBROUTINE SUB_PASS1(X)
        IMPLICIT NONE
        INTEGER :: X
        END SUBROUTINE SUB_PASS1
    
        SUBROUTINE SUB_PASS2(X)
        IMPLICIT NONE
        CLASS(T) :: X
        END SUBROUTINE SUB_PASS2
END PROGRAM P
#endif
