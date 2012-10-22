! <testinfo>
! test_generator=config/mercurium-fortran
! compile_versions="mod use all"
! test_FFLAGS_mod="-DWRITE_MOD"
! test_FFLAGS_use="-DUSE_MOD"
! test_FFLAGS_all="-DWRITE_MOD -DUSE_MOD"
! </testinfo>

#ifdef WRITE_MOD
MODULE M
    IMPLICIT NONE
    INTEGER :: GLOBAL_DECOMPOSITION
END MODULE M
#endif

#ifdef USE_MOD
SUBROUTINE S()
    USE M, ONLY: GLOBAL_DECOMPOSITION
    IMPLICIT NONE
    INTEGER :: Y
    Y = GLOBAL_DECOMPOSITION
CONTAINS
    SUBROUTINE T()
        USE M, ONLY: GL_DC=> GLOBAL_DECOMPOSITION
    IMPLICIT NONE
        INTEGER :: X
        X = GL_DC
    END SUBROUTINE
END SUBROUTINE
#endif
