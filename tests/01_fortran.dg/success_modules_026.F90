! <testinfo>
! test_generator=config/mercurium-fortran
! compile_versions="cache nocache"
! test_FFLAGS_cache=""
! test_FFLAGS_nocache="--debug-flags=disable_module_cache"
! </testinfo>
MODULE M
    INTERFACE
        SUBROUTINE S(X)
            INTEGER :: X
        END SUBROUTINE S
    END INTERFACE

CONTAINS

    SUBROUTINE S1
        CALL S(X = 1)
    END SUBROUTINE S1

END MODULE M
