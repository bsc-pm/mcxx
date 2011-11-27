! <testinfo>
! test_generator=config/mercurium-fortran
! compile_versions="cache nocache"
! test_FFLAGS_cache=""
! test_FFLAGS_nocache="--debug-flags=disable_module_cache"
! </testinfo>
MODULE M1
    IMPLICIT NONE
    TYPE T
        INTEGER :: X
    END TYPE T
END MODULE M1
MODULE M2
    USE M1
    IMPLICIT NONE
    TYPE K
       TYPE(T) :: GLOBAL_T
    END TYPE K
END MODULE M2
MODULE M3
    USE M1
    USE M2
END MODULE M3
MODULE M4
    USE M3
END MODULE M4
