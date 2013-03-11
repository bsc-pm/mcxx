! <testinfo>
! test_generator=config/mercurium-fortran
! compile_versions="mod mod2 mod3 all"
! test_FFLAGS_mod="-DWRITE_MOD_A"
! test_FFLAGS_mod2="-DWRITE_MOD_B"
! test_FFLAGS_mod3="-DWRITE_MOD_C"
! test_FFLAGS_all="-DWRITE_MOD_A -DWRITE_MOD_B -DWRITE_MOD_C"
! </testinfo>
#ifdef WRITE_MOD_A
MODULE M
    PRIVATE
    INTEGER, PARAMETER, PUBLIC :: PARAM = 10
END MODULE M
#endif

#ifdef WRITE_MOD_B
MODULE M2
    USE M, ONLY : PARAM
    PRIVATE
    INTEGER :: FOO
END MODULE M2
#endif

#ifdef WRITE_MOD_C
MODULE M3
    USE M2
    PRIVATE
    INTEGER, PARAMETER :: PARAM = 10
END MODULE M3
#endif
