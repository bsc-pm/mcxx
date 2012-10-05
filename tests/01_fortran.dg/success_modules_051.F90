! <testinfo>
! test_generator=config/mercurium-fortran
! compile_versions="cache nocache"
! test_FFLAGS_cache=""
! test_FFLAGS_nocache="--debug-flags=disable_module_cache"
! </testinfo>
MODULE M
    PRIVATE

    INTERFACE FOO
        MODULE PROCEDURE FOO
    END INTERFACE FOO

    CONTAINS

        SUBROUTINE FOO(X)
            INTEGER :: X
        END SUBROUTINE FOO
END MODULE M
