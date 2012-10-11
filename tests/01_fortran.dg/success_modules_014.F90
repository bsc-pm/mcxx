! <testinfo>
! test_generator=config/mercurium-fortran
! compile_versions="mod use all"
! test_FFLAGS_mod="-DWRITE_MOD"
! test_FFLAGS_use="-DUSE_MOD"
! test_FFLAGS_all="-DWRITE_MOD -DUSE_MOD"
! </testinfo>
#ifdef WRITE_MOD
MODULE M
    CHARACTER(LEN=10), PARAMETER :: C = "HOWDY"
END MODULE M
#endif

#ifdef USE_MOD
PROGRAM P
    USE M
    IMPLICIT NONE
    CHARACTER(LEN=10) :: X = C ! Error here only if 
                               ! M is compiled separatedly
    PRINT *, C
    PRINT *, X
END PROGRAM P
#endif
