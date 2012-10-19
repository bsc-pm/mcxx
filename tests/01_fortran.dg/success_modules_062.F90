! <testinfo>
! test_generator=config/mercurium-fortran
! compile_versions="mod use all"
! test_FFLAGS_mod="-DWRITE_MOD"
! test_FFLAGS_use="-DUSE_MOD"
! test_FFLAGS_all="-DWRITE_MOD -DUSE_MOD"
! </testinfo>

#ifdef WRITE_MOD
MODULE M
    INTEGER :: X, Y
    NAMELIST /NL/ X, Y
END MODULE M
#endif

#ifdef USE_MOD
PROGRAM P
USE M, ONLY: X, Y, NL
IMPLICIT NONE
    X = 1
    Y = -1
    OPEN(UNIT=12, FILE="FOO.TXT")
    WRITE(UNIT=12, NML=NL)
    CLOSE(UNIT=12)
END PROGRAM P
#endif
