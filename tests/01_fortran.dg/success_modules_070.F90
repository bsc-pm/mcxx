! <testinfo>
! test_generator=config/mercurium-fortran
! compile_versions="mod mod2 all"
! test_FFLAGS_mod="-DWRITE_MOD_A"
! test_FFLAGS_mod2="-DWRITE_MOD_B"
! test_FFLAGS_all="-DWRITE_MOD_A -DWRITE_MOD_B"
! </testinfo>
#ifdef WRITE_MOD_A
MODULE M1
    PRIVATE
    INTRINSIC :: NULL
END MODULE M1
#endif

#ifdef WRITE_MOD_B
MODULE M2

CONTAINS
    SUBROUTINE S2
        USE M1
        INTEGER, POINTER :: P
        INTRINSIC :: NULL

        P => NULL()
    END SUBROUTINE S2
END MODULE M2
#endif
