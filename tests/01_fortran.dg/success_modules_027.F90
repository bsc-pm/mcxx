! <testinfo>
! test_generator=config/mercurium-fortran
! compile_versions="cache nocache"
! test_FFLAGS_cache=""
! test_FFLAGS_nocache="--debug-flags=disable_module_cache"
! </testinfo>
MODULE M1
    INTEGER :: X, Y
END MODULE M1

MODULE M2
    USE M1, ONLY : X, Y

    PRIVATE

    PUBLIC :: X
    ! Y must be PRIVATE afterwards
END MODULE M2
