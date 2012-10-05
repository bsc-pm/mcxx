! <testinfo>
! test_generator=config/mercurium-fortran
! compile_versions="mod use all"
! test_FFLAGS_mod="-DWRITE_MOD"
! test_FFLAGS_use="-DUSE_MOD"
! test_FFLAGS_all="-DWRITE_MOD -DUSE_MOD"
! </testinfo>

#ifdef WRITE_MOD
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
#endif

#ifdef USE_MOD
PROGRAM P
    USE M
END PROGRAM P
#endif
