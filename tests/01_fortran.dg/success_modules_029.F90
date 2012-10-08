! <testinfo>
! test_generator=config/mercurium-fortran
! compile_versions="mod use all"
! test_FFLAGS_mod="-DWRITE_MOD"
! test_FFLAGS_use="-DUSE_MOD"
! test_FFLAGS_all="-DWRITE_MOD -DUSE_MOD"
! </testinfo>

#ifdef WRITE_MOD
MODULE M
    CONTAINS
        SUBROUTINE S2(X)
            INTEGER :: X
        END SUBROUTINE S2
END MODULE M
#endif

#ifdef USE_MOD
SUBROUTINE P1
    USE M
    IMPLICIT NONE

    CONTAINS
        SUBROUTINE S
            IMPLICIT NONE
            CALL S2(3)
        END SUBROUTINE S
END 

SUBROUTINE P2
    IMPLICIT NONE

    CONTAINS
        SUBROUTINE S
            USE M
            IMPLICIT NONE
            CALL S2(3)
        END SUBROUTINE S
END 

SUBROUTINE P3
    IMPLICIT NONE
    USE M

    CONTAINS
        SUBROUTINE S
            USE M
            IMPLICIT NONE
            CALL S2(3)
        END SUBROUTINE S
END 
#endif
