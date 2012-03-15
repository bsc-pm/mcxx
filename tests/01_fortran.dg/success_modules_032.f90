! <testinfo>
! test_generator=config/mercurium-fortran
! compile_versions="cache nocache"
! test_FFLAGS_cache=""
! test_FFLAGS_nocache="--debug-flags=disable_module_cache"
! </testinfo>
MODULE M
    CONTAINS

        SUBROUTINE S(F)
            INTERFACE
                SUBROUTINE F(V, X)
                    INTEGER :: X
                    INTEGER :: V(X)
                END SUBROUTINE F
            END INTERFACE
       END SUBROUTINE S
END MODULE M

PROGRAM P
    USE M
END PROGRAM P
