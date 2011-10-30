! <testinfo>
! test_generator=config/mercurium-fortran
! compile_versions="cache nocache"
! test_FFLAGS_cache=""
! test_FFLAGS_nocache="--debug-flags=disable_module_cache"
! </testinfo>
MODULE FOO
    IMPLICIT NONE
     INTEGER, PARAMETER :: K = 10
END MODULE FOO
MODULE BAR
    USE FOO
    IMPLICIT NONE
    TYPE T
         INTEGER :: X = K
    END TYPE T
END MODULE BAR
PROGRAM P
    USE BAR
    IMPLICIT NONE
    TYPE(T) :: S
    PRINT *, S
END PROGRAM P
