! <testinfo>
! test_generator=config/mercurium-fortran
! compile_versions="mod mod2 mod3 all"
! test_FFLAGS_mod="-DWRITE_MOD_A"
! test_FFLAGS_mod2="-DWRITE_MOD_B"
! test_FFLAGS_mod3="-DWRITE_MOD_C"
! test_FFLAGS_all="-DWRITE_MOD_A -DWRITE_MOD_B -DWRITE_MOD_C"
! </testinfo>

#ifdef WRITE_MOD_A
MODULE MOO
    INTEGER :: A
END MODULE MOO
#endif

#ifdef WRITE_MOD_B
MODULE BAR
    USE MOO, ONLY : F => A
END MODULE BAR
#endif

#ifdef WRITE_MOD_C
MODULE C
    USE BAR
    CONTAINS
        SUBROUTINE S()
            USE MOO, ONLY : F => A
        END SUBROUTINE S
END MODULE C
#endif
