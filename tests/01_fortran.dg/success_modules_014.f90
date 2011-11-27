! <testinfo>
! test_generator=config/mercurium-fortran
! compile_versions="cache nocache"
! test_FFLAGS_cache=""
! test_FFLAGS_nocache="--debug-flags=disable_module_cache"
! </testinfo>
MODULE M
    CHARACTER(LEN=10), PARAMETER :: C = "HOWDY"
END MODULE M

PROGRAM P
    USE M
    IMPLICIT NONE
    CHARACTER(LEN=10) :: X = C ! Error here only if 
                               ! M is compiled separatedly
    PRINT *, C
    PRINT *, X
END PROGRAM P
