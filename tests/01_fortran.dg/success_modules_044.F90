! <testinfo>
! test_generator=config/mercurium-fortran
! compile_versions="cache nocache"
! test_FFLAGS_cache=""
! test_FFLAGS_nocache="--debug-flags=disable_module_cache"
! </testinfo>
MODULE A
    CONTAINS

        SUBROUTINE SUB_A(X, Y)
            INTEGER :: X, Y
            OPTIONAL :: Y
        END SUBROUTINE SUB_A

END MODULE A

MODULE B
    USE A
END MODULE A

PROGRAM P
    USE B

    CALL SUB_A(1)
END PROGRAM P
